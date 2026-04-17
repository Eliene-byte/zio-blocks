package zio.schema.migration

import zio.schema.{DynamicValue, Schema}
import zio.{Chunk, NonEmptyChunk}
import scala.annotation.tailrec

// ============================================================================
// Core Algebra - Zero-dependency, Total, Serializable
// ============================================================================

sealed trait MigrationError extends Product with Serializable
object MigrationError {
  final case class FieldNotFound(path: DynamicOptic, available: List[String]) extends MigrationError
  final case class TypeMismatch(path: DynamicOptic, expected: String, actual: String) extends MigrationError
  final case class EvaluationFailed(path: DynamicOptic, cause: String) extends MigrationError
  final case class InvalidPath(path: DynamicOptic) extends MigrationError

  def toMessage(error: MigrationError): String = error match {
    case FieldNotFound(path, available) =>
      s"Field not found at ${path.render}. Available: ${available.mkString(", ")}"
    case TypeMismatch(path, exp, act) =>
      s"Type mismatch at ${path.render}. Expected: $exp, Actual: $act"
    case EvaluationFailed(path, cause) =>
      s"Evaluation failed at ${path.render}: $cause"
    case InvalidPath(path) =>
      s"Invalid path: ${path.render}"
  }
}

sealed trait DynamicOptic extends Product with Serializable {
  def render: String
  def append(segment: OpticSegment): DynamicOptic
}
object DynamicOptic {
  case object Root extends DynamicOptic {
    def render: String = "$"
    def append(segment: OpticSegment): DynamicOptic = segment match {
      case OpticSegment.Field(name) => Field(Root, name)
      case OpticSegment.Index(idx) => Index(Root, idx)
      case OpticSegment.Each => Each(Root)
      case OpticSegment.When(tag) => When(Root, tag)
    }
  }
  final case class Field(parent: DynamicOptic, name: String) extends DynamicOptic {
    def render: String = s"${parent.render}.$name"
    def append(segment: OpticSegment): DynamicOptic = segment match {
      case OpticSegment.Field(n) => Field(this, n)
      case OpticSegment.Index(idx) => Index(this, idx)
      case OpticSegment.Each => Each(this)
      case OpticSegment.When(tag) => When(this, tag)
    }
  }
  final case class Index(parent: DynamicOptic, index: Int) extends DynamicOptic {
    def render: String = s"${parent.render}[$index]"
    def append(segment: OpticSegment): DynamicOptic = parent.append(segment)
  }
  final case class Each(parent: DynamicOptic) extends DynamicOptic {
    def render: String = s"${parent.render}[*]"
    def append(segment: OpticSegment): DynamicOptic = parent.append(segment)
  }
  final case class When(parent: DynamicOptic, tag: String) extends DynamicOptic {
    def render: String = s"${parent.render} when $tag"
    def append(segment: OpticSegment): DynamicOptic = parent.append(segment)
  }
}

sealed trait OpticSegment
object OpticSegment {
  final case class Field(name: String) extends OpticSegment
  final case class Index(index: Int) extends OpticSegment
  case object Each extends OpticSegment
  final case class When(tag: String) extends OpticSegment
}

sealed trait SchemaExpr[A] extends Product with Serializable {
  def evaluate(value: DynamicValue): Either[MigrationError, DynamicValue]
  def reverse: SchemaExpr[Any]
}
object SchemaExpr {
  final case class Const[A](value: A, schema: Schema[A]) extends SchemaExpr[A] {
    def evaluate(v: DynamicValue): Either[MigrationError, DynamicValue] =
      Right(DynamicValue.Primitive(value, schema))
    def reverse: SchemaExpr[Any] = Const(value, schema)
  }

  case object DefaultValue extends SchemaExpr[Nothing] {
    def evaluate(v: DynamicValue): Either[MigrationError, DynamicValue] =
      Left(MigrationError.EvaluationFailed(DynamicOptic.Root, "DefaultValue requires schema context"))
    def reverse: SchemaExpr[Any] = DefaultValue
  }

  final case class Concat extends SchemaExpr[(String, String)] {
    def evaluate(v: DynamicValue): Either[MigrationError, DynamicValue] = v match {
      case DynamicValue.Tuple2(DynamicValue.Primitive(s1: String, _), DynamicValue.Primitive(s2: String, _)) =>
        Right(DynamicValue.Primitive(s"$s1 $s2", Schema[String]))
      case other => Left(MigrationError.TypeMismatch(DynamicOptic.Root, "(String, String)", other.toString))
    }
    def reverse: SchemaExpr[Any] = Split
  }

  case object Split extends SchemaExpr[String] {
    def evaluate(v: DynamicValue): Either[MigrationError, DynamicValue] = v match {
      case DynamicValue.Primitive(s: String, _) =>
        s.split(" ", 2) match {
          case Array(a, b) => Right(DynamicValue.Tuple2(
            DynamicValue.Primitive(a, Schema[String]),
            DynamicValue.Primitive(b, Schema[String])
          ))
          case _ => Left(MigrationError.EvaluationFailed(DynamicOptic.Root, "Cannot split"))
        }
      case other => Left(MigrationError.TypeMismatch(DynamicOptic.Root, "String", other.toString))
    }
    def reverse: SchemaExpr[Any] = Concat()
  }
}

sealed trait MigrationAction extends Product with Serializable {
  def at: DynamicOptic
  def reverse: MigrationAction
}
object MigrationAction {
  final case class AddField(at: DynamicOptic, name: String, default: SchemaExpr[_]) extends MigrationAction {
    def reverse: MigrationAction = DropField(at, name, default)
  }
  final case class DropField(at: DynamicOptic, name: String, defaultForReverse: SchemaExpr[_]) extends MigrationAction {
    def reverse: MigrationAction = AddField(at, name, defaultForReverse)
  }
  final case class Rename(at: DynamicOptic, from: String, to: String) extends MigrationAction {
    def reverse: MigrationAction = Rename(at, to, from)
  }
  final case class TransformValue(at: DynamicOptic, transform: SchemaExpr[_]) extends MigrationAction {
    def reverse: MigrationAction = TransformValue(at, transform.reverse)
  }
  final case class Mandate(at: DynamicOptic, field: String, default: SchemaExpr[_]) extends MigrationAction {
    def reverse: MigrationAction = Optionalize(at, field)
  }
  final case class Optionalize(at: DynamicOptic, field: String) extends MigrationAction {
    def reverse: MigrationAction = Mandate(at, field, SchemaExpr.DefaultValue)
  }
  final case class Join(at: DynamicOptic, from1: String, from2: String, to: String, joinFn: SchemaExpr[_]) extends MigrationAction {
    def reverse: MigrationAction = Split(at, to, from1, from2, joinFn.reverse)
  }
  final case class Split(at: DynamicOptic, from: String, to1: String, to2: String, splitFn: SchemaExpr[_]) extends MigrationAction {
    def reverse: MigrationAction = Join(at, to1, to2, from, splitFn.reverse)
  }
  final case class ChangeType(at: DynamicOptic, field: String, converter: SchemaExpr[_]) extends MigrationAction {
    def reverse: MigrationAction = ChangeType(at, field, converter.reverse)
  }
  final case class RenameCase(at: DynamicOptic, from: String, to: String) extends MigrationAction {
    def reverse: MigrationAction = RenameCase(at, to, from)
  }
  final case class TransformCase(at: DynamicOptic, tag: String, actions: Vector[MigrationAction]) extends MigrationAction {
    def reverse: MigrationAction = TransformCase(at, tag, actions.map(_.reverse).reverse)
  }
  final case class TransformElements(at: DynamicOptic, transform: SchemaExpr[_]) extends MigrationAction {
    def reverse: MigrationAction = TransformElements(at, transform.reverse)
  }
  final case class TransformKeys(at: DynamicOptic, transform: SchemaExpr[_]) extends MigrationAction {
    def reverse: MigrationAction = TransformKeys(at, transform.reverse)
  }
  final case class TransformValues(at: DynamicOptic, transform: SchemaExpr[_]) extends MigrationAction {
    def reverse: MigrationAction = TransformValues(at, transform.reverse)
  }
}

final case class DynamicMigration(actions: Vector[MigrationAction]) {
  def apply(value: DynamicValue): Either[MigrationError, DynamicValue] =
    actions.foldLeft[Either[MigrationError, DynamicValue]](Right(value)) { (acc, action) =>
      acc.flatMap(applyAction(_, action))
    }

  def ++(that: DynamicMigration): DynamicMigration =
    DynamicMigration(actions ++ that.actions)

  def reverse: DynamicMigration =
    DynamicMigration(actions.reverse.map(_.reverse))

  private def applyAction(value: DynamicValue, action: MigrationAction): Either[MigrationError, DynamicValue] = {
    def navigate(optic: DynamicOptic, v: DynamicValue): Either[MigrationError, DynamicValue] =
      optic match {
        case DynamicOptic.Root => Right(v)
        case DynamicOptic.Field(parent, name) =>
          navigate(parent, v).flatMap {
            case DynamicValue.Record(values, schema) =>
              values.find(_._1 == name) match {
                case Some((_, fieldValue)) => Right(fieldValue)
                case None => Left(MigrationError.FieldNotFound(optic, values.map(_._1).toList))
              }
            case other => Left(MigrationError.TypeMismatch(optic, "Record", other.getClass.getSimpleName))
          }
        case DynamicOptic.Index(parent, idx) =>
          navigate(parent, v).flatMap {
            case DynamicValue.Sequence(values, _) =>
              values.lift(idx).toRight(MigrationError.InvalidPath(optic))
            case other => Left(MigrationError.TypeMismatch(optic, "Sequence", other.getClass.getSimpleName))
          }
        case DynamicOptic.Each(parent) =>
          navigate(parent, v)
        case DynamicOptic.When(parent, tag) =>
          navigate(parent, v).flatMap {
            case enum @ DynamicValue.Enumeration(currentTag, value, _) if currentTag == tag => Right(value)
            case DynamicValue.Enumeration(currentTag, _, _) =>
              Left(MigrationError.InvalidPath(optic))
            case other => Left(MigrationError.TypeMismatch(optic, "Enumeration", other.getClass.getSimpleName))
          }
      }

    def updateAt(optic: DynamicOptic, v: DynamicValue, f: DynamicValue => Either[MigrationError, DynamicValue]): Either[MigrationError, DynamicValue] =
      optic match {
        case DynamicOptic.Root => f(v)
        case DynamicOptic.Field(parent, name) =>
          updateAt(parent, v, parentValue => parentValue match {
            case DynamicValue.Record(values, schema) =>
              val idx = values.indexWhere(_._1 == name)
              if (idx >= 0) {
                f(values(idx)._2).map(newVal =>
                  DynamicValue.Record(values.updated(idx, name -> newVal), schema))
              } else Left(MigrationError.FieldNotFound(optic, values.map(_._1).toList))
            case other => Left(MigrationError.TypeMismatch(optic, "Record", other.getClass.getSimpleName))
          })
        case _ => Left(MigrationError.InvalidPath(optic))
      }

    action match {
      case MigrationAction.AddField(at, name, default) =>
        updateAt(at, value, {
          case r @ DynamicValue.Record(values, schema) =>
            if (values.exists(_._1 == name)) Right(r)
            else default.evaluate(DynamicValue.Record.empty).map(dv =>
              DynamicValue.Record(values :+ (name -> dv), schema))
          case other => Left(MigrationError.TypeMismatch(at, "Record", other.getClass.getSimpleName))
        })

      case MigrationAction.DropField(at, name, _) =>
        updateAt(at, value, {
          case DynamicValue.Record(values, schema) =>
            Right(DynamicValue.Record(values.filterNot(_._1 == name), schema))
          case other => Left(MigrationError.TypeMismatch(at, "Record", other.getClass.getSimpleName))
        })

      case MigrationAction.Rename(at, from, to) =>
        updateAt(at, value, {
          case DynamicValue.Record(values, schema) =>
            val idx = values.indexWhere(_._1 == from)
            if (idx >= 0) {
              val (name, v) = values(idx)
              Right(DynamicValue.Record(values.updated(idx, to -> v), schema))
            } else Left(MigrationError.FieldNotFound(at, values.map(_._1).toList))
          case other => Left(MigrationError.TypeMismatch(at, "Record", other.getClass.getSimpleName))
        })

      case MigrationAction.TransformValue(at, transform) =>
        updateAt(at, value, v => transform.evaluate(v))

      case MigrationAction.Mandate(at, field, default) =>
        updateAt(at, value, {
          case DynamicValue.Record(values, schema) =>
            values.find(_._1 == field) match {
              case Some((_, DynamicValue.NoneValue)) =>
                default.evaluate(DynamicValue.Record.empty).map(dv =>
                  DynamicValue.Record(values.updated(values.indexWhere(_._1 == field), field -> dv), schema))
              case Some((_, DynamicValue.SomeValue(v))) =>
                Right(DynamicValue.Record(values.updated(values.indexWhere(_._1 == field), field -> v), schema))
              case _ => Right(DynamicValue.Record(values, schema))
            }
          case other => Left(MigrationError.TypeMismatch(at, "Record", other.getClass.getSimpleName))
        })

      case MigrationAction.Optionalize(at, field) =>
        updateAt(at, value, {
          case DynamicValue.Record(values, schema) =>
            val idx = values.indexWhere(_._1 == field)
            if (idx >= 0) {
              val (_, v) = values(idx)
              Right(DynamicValue.Record(values.updated(idx, field -> DynamicValue.SomeValue(v)), schema))
            } else Right(DynamicValue.Record(values, schema))
          case other => Left(MigrationError.TypeMismatch(at, "Record", other.getClass.getSimpleName))
        })

      case MigrationAction.Join(at, f1, f2, to, joinFn) =>
        updateAt(at, value, {
          case DynamicValue.Record(values, schema) =>
            for {
              v1 <- values.find(_._1 == f1).map(_._2).toRight(MigrationError.FieldNotFound(at, List(f1)))
              v2 <- values.find(_._1 == f2).map(_._2).toRight(MigrationError.FieldNotFound(at, List(f2)))
              joined <- joinFn.evaluate(DynamicValue.Tuple2(v1, v2))
              filtered = values.filterNot(x => x._1 == f1 || x._1 == f2)
            } yield DynamicValue.Record(filtered :+ (to -> joined), schema)
          case other => Left(MigrationError.TypeMismatch(at, "Record", other.getClass.getSimpleName))
        })

      case MigrationAction.Split(at, from, t1, t2, splitFn) =>
        updateAt(at, value, {
          case DynamicValue.Record(values, schema) =>
            for {
              v <- values.find(_._1 == from).map(_._2).toRight(MigrationError.FieldNotFound(at, List(from)))
              split <- splitFn.evaluate(v)
              (v1, v2) <- split match {
                case DynamicValue.Tuple2(a, b) => Right((a, b))
                case other => Left(MigrationError.TypeMismatch(at, "Tuple2", other.getClass.getSimpleName))
              }
              filtered = values.filterNot(_._1 == from)
            } yield DynamicValue.Record(filtered ++ List(t1 -> v1, t2 -> v2), schema)
          case other => Left(MigrationError.TypeMismatch(at, "Record", other.getClass.getSimpleName))
        })

      case MigrationAction.ChangeType(at, field, converter) =>
        updateAt(at, value, {
          case DynamicValue.Record(values, schema) =>
            val idx = values.indexWhere(_._1 == field)
            if (idx >= 0) {
              converter.evaluate(values(idx)._2).map(newVal =>
                DynamicValue.Record(values.updated(idx, field -> newVal), schema))
            } else Left(MigrationError.FieldNotFound(at, values.map(_._1).toList))
          case other => Left(MigrationError.TypeMismatch(at, "Record", other.getClass.getSimpleName))
        })

      case MigrationAction.RenameCase(at, from, to) =>
        updateAt(at, value, {
          case DynamicValue.Enumeration(tag, v, schema) if tag == from =>
            Right(DynamicValue.Enumeration(to, v, schema))
          case e @ DynamicValue.Enumeration(_, _, _) => Right(e)
          case other => Left(MigrationError.TypeMismatch(at, "Enumeration", other.getClass.getSimpleName))
        })

      case MigrationAction.TransformCase(at, tag, actions) =>
        updateAt(at, value, {
          case enum @ DynamicValue.Enumeration(currentTag, v, schema) if currentTag == tag =>
            DynamicMigration(actions)(v).map(newV => DynamicValue.Enumeration(tag, newV, schema))
          case e @ DynamicValue.Enumeration(_, _, _) => Right(e)
          case other => Left(MigrationError.TypeMismatch(at, "Enumeration", other.getClass.getSimpleName))
        })

      case MigrationAction.TransformElements(at, transform) =>
        updateAt(at, value, {
          case DynamicValue.Sequence(values, schema) =>
            values.map(transform.evaluate).sequence.map(DynamicValue.Sequence(_, schema))
          case other => Left(MigrationError.TypeMismatch(at, "Sequence", other.getClass.getSimpleName))
        })

      case MigrationAction.TransformKeys(at, transform) =>
        updateAt(at, value, {
          case DynamicValue.Dictionary(entries, schema) =>
            entries.map { case (k, v) => transform.evaluate(k).map(_ -> v) }.sequence
             .map(DynamicValue.Dictionary(_, schema))
          case other => Left(MigrationError.TypeMismatch(at, "Dictionary", other.getClass.getSimpleName))
        })

      case MigrationAction.TransformValues(at, transform) =>
        updateAt(at, value, {
          case DynamicValue.Dictionary(entries, schema) =>
            entries.map { case (k, v) => transform.evaluate(v).map(k -> _) }.sequence
             .map(DynamicValue.Dictionary(_, schema))
          case other => Left(MigrationError.TypeMismatch(at, "Dictionary", other.getClass.getSimpleName))
        })
    }
  }

  implicit class EitherSeqOps[A, B](seq: Seq[Either[A, B]]) {
    def sequence: Either[A, Seq[B]] =
      seq.foldLeft[Either[A, Vector[B]]](Right(Vector.empty)) { (acc, e) =>
        for { xs <- acc; x <- e } yield xs :+ x
      }
  }
}

final case class Migration[A, B](
  dynamicMigration: DynamicMigration,
  sourceSchema: Schema[A],
  targetSchema: Schema[B]
) {
  def apply(value: A): Either[MigrationError, B] =
    for {
      dynValue <- sourceSchema.toDynamic(value).left.map(e =>
        MigrationError.EvaluationFailed(DynamicOptic.Root, e))
      migrated <- dynamicMigration(dynValue)
      result <- targetSchema.fromDynamic(migrated).left.map(e =>
        MigrationError.EvaluationFailed(DynamicOptic.Root, e))
    } yield result

  def ++[C](that: Migration[B, C]): Migration[A, C] =
    Migration(dynamicMigration ++ that.dynamicMigration, sourceSchema, that.targetSchema)

  def reverse: Migration[B, A] =
    Migration(dynamicMigration.reverse, targetSchema, sourceSchema)
}

object Migration {
  def identity[A](implicit schema: Schema[A]): Migration[A, A] =
    Migration(DynamicMigration(Vector.empty), schema, schema)

  def newBuilder[A, B](implicit src: Schema[A], tgt: Schema[B]): MigrationBuilder[A, B] =
    MigrationBuilder(src, tgt, Vector.empty)
}

final case class MigrationBuilder[A, B](
  sourceSchema: Schema[A],
  targetSchema: Schema[B],
  actions: Vector[MigrationAction]
) {
  def addField(target: B => Any, name: String, default: SchemaExpr[_]): MigrationBuilder[A, B] =
    copy(actions = actions :+ MigrationAction.AddField(DynamicOptic.Root, name, default))

  def dropField(source: A => Any, name: String, defaultForReverse: SchemaExpr[_]): MigrationBuilder[A, B] =
    copy(actions = actions :+ MigrationAction.DropField(DynamicOptic.Root, name, defaultForReverse))

  def renameField(from: String, to: String): MigrationBuilder[A, B] =
    copy(actions = actions :+ MigrationAction.Rename(DynamicOptic.Root, from, to))

  def transformField(field: String, transform: SchemaExpr[_]): MigrationBuilder[A, B] =
    copy(actions = actions :+ MigrationAction.TransformValue(DynamicOptic.Field(DynamicOptic.Root, field), transform))

  def mandateField(field: String, default: SchemaExpr[_]): MigrationBuilder[A, B] =
    copy(actions = actions :+ MigrationAction.Mandate(DynamicOptic.Root, field, default))

  def optionalizeField(field: String): MigrationBuilder[A, B] =
    copy(actions = actions :+ MigrationAction.Optionalize(DynamicOptic.Root, field))

  def join(f1: String, f2: String, to: String, joinFn: SchemaExpr[_]): MigrationBuilder[A, B] =
    copy(actions = actions :+ MigrationAction.Join(DynamicOptic.Root, f1, f2, to, joinFn))

  def split(from: String, t1: String, t2: String, splitFn: SchemaExpr[_]): MigrationBuilder[A, B] =
    copy(actions = actions :+ MigrationAction.Split(DynamicOptic.Root, from, t1, t2, splitFn))

  def changeType(field: String, converter: SchemaExpr[_]): MigrationBuilder[A, B] =
    copy(actions = actions :+ MigrationAction.ChangeType(DynamicOptic.Root, field, converter))

  def renameCase(from: String, to: String): MigrationBuilder[A, B] =
    copy(actions = actions :+ MigrationAction.RenameCase(DynamicOptic.Root, from, to))

  def transformCase(tag: String, f: MigrationBuilder[Any, Any] => MigrationBuilder[Any, Any]): MigrationBuilder[A, B] = {
    val subBuilder = f(MigrationBuilder[Any, Any](Schema[Any], Schema[Any], Vector.empty))
    copy(actions = actions :+ MigrationAction.TransformCase(DynamicOptic.Root, tag, subBuilder.actions))
  }

  def transformElements(field: String, transform: SchemaExpr[_]): MigrationBuilder[A, B] =
    copy(actions = actions :+ MigrationAction.TransformElements(DynamicOptic.Field(DynamicOptic.Root, field), transform))

  def transformKeys(field: String, transform: SchemaExpr[_]): MigrationBuilder[A, B] =
    copy(actions = actions :+ MigrationAction.TransformKeys(DynamicOptic.Field(DynamicOptic.Root, field), transform))

  def transformValues(field: String, transform: SchemaExpr[_]): MigrationBuilder[A, B] =
    copy(actions = actions :+ MigrationAction.TransformValues(DynamicOptic.Field(DynamicOptic.Root, field), transform))

  def build: Migration[A, B] = Migration(DynamicMigration(actions), sourceSchema, targetSchema)
}