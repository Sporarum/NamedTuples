import language.dynamics

type LabelType = String & Singleton

// Named pair constructor
object P extends Dynamic:
  transparent inline def applyDynamicNamed[LA <: LabelType, A, LB <: LabelType, B, T <: Tuple](inline s: "apply")(inline a: (LA, A), b: (LB, B)): Any =
    NamedPair[LA, A, LB, B](a._2, b._2)


class NamedPair[LabelA <: LabelType, A, LabelB <: LabelType, B](val _1: A, val _2: B) extends Dynamic:

  transparent inline def selectDynamic(inline s: String) =
    inline s match
      case _ : LabelA => _1
      case _ : LabelB => _2
      case _ =>
        compiletime.error("Field does not exist")


given [LabelA <: LabelType, A, LabelB <: LabelType, B]: Conversion[(A, B), NamedPair[LabelA, A, LabelB, B]] = (a, b) => NamedPair(a, b)

@main def testPair: Unit =
  val john = P(age = 42, name = "John")

  john.age
  john.name

  //john.address // error: Field does not exist

  type Person = NamedPair["age", Int, "name", String]

  val bill: Person = (3, "Bill")

  val john2: Person = john
