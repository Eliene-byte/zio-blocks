package zio.schema.migration

import zio.test._
import zio.schema.{DeriveSchema, Schema}

object MigrationSpec extends ZIOSpecDefault {
  case class PersonV0(firstName: String, lastName: String)
  case class PersonV1(firstName: String, lastName: String, age: Int)
  case class PersonV2(fullName: String, age: Int)

  implicit val schemaV0: Schema = DeriveSchema.gen
  implicit val schemaV1: Schema = DeriveSchema.gen
  implicit val schemaV2: Schema = DeriveSchema.gen

  sealed trait Status
  case class Active(since: Long) extends Status
  case class Inactive(reason: String) extends Status
  implicit val schemaStatus: Schema[Status] = DeriveSchema.gen[Status]

  def spec = suite("MigrationSpec")(
    test("AddField") {
      val m = Migration.newBuilder[PersonV0, PersonV1]
       .addField(_.age, "age", SchemaExpr.Const(0, Schema[Int]))
       .build
      assertTrue(m(PersonV0("John", "Doe")) == Right(PersonV1("John", "Doe", 0)))
    },
    test("RenameField") {
      val m = Migration.newBuilder[PersonV0, PersonV0]
       .renameField("firstName", "name")
       .build
      val result = m(PersonV0("John", "Doe"))
      assertTrue(result.isRight)
    },
    test("Join fields") {
      val m = Migration.newBuilder[PersonV0, PersonV2]
       .addField(_.age, "age", SchemaExpr.Const(0, Schema[Int]))
       .join("firstName", "lastName", "fullName", SchemaExpr.Concat())
       .dropField(_.firstName, "firstName", SchemaExpr.Const("", Schema[String]))
       .dropField(_.lastName, "lastName", SchemaExpr.Const("", Schema[String]))
       .build
      assertTrue(m(PersonV0("John", "Doe")) == Right(PersonV2("John Doe", 0)))
    },
    test("Split field") {
      val m = Migration.newBuilder[PersonV2, PersonV0]
       .split("fullName", "firstName", "lastName", SchemaExpr.Split)
       .dropField(_.age, "age", SchemaExpr.Const(0, Schema[Int]))
       .build
      assertTrue(m(PersonV2("John Doe", 30)) == Right(PersonV0("John", "Doe")))
    },
    test("TransformElements") {
      case class Team(members: Vector[String])
      implicit val schema: Schema[Team] = DeriveSchema.gen[Team]
      val m = Migration.newBuilder[Team, Team]
       .transformElements("members", SchemaExpr.Const("x", Schema[String]))
       .build
      assertTrue(m(Team(Vector("a", "b"))) == Right(Team(Vector("x", "x"))))
    },
    test("TransformValues on Map") {
      case class Data(values: Map[String, Int])
      implicit val schema: Schema[Data] = DeriveSchema.gen[Data]
      val m = Migration.newBuilder[Data, Data]
       .transformValues("values", SchemaExpr.Const(1, Schema[Int]))
       .build
      assertTrue(m(Data(Map("a" -> 0, "b" -> 0))) == Right(Data(Map("a" -> 1, "b" -> 1))))
    },
    test("Reverse migration") {
      val m1 = Migration.newBuilder[PersonV0, PersonV1]
       .addField(_.age, "age", SchemaExpr.Const(0, Schema[Int]))
       .build
      val m2 = m1.reverse
      val v1 = PersonV1("John", "Doe", 30)
      assertTrue(m2(v1) == Right(PersonV0("John", "Doe")))
    },
    test("Composition") {
      val m1 = Migration.newBuilder[PersonV0, PersonV1]
       .addField(_.age, "age", SchemaExpr.Const(0, Schema[Int]))
       .build
      val m2 = Migration.newBuilder[PersonV1, PersonV2]
       .join("firstName", "lastName", "fullName", SchemaExpr.Concat())
       .dropField(_.firstName, "firstName", SchemaExpr.Const("", Schema[String]))
       .dropField(_.lastName, "lastName", SchemaExpr.Const("", Schema[String]))
       .build
      val composed = m1 ++ m2
      assertTrue(composed(PersonV0("John", "Doe")) == Right(PersonV2("John Doe", 0)))
    }
  )
}