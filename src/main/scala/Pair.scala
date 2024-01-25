import language.dynamics

package Tuple:

  type LabelType = String & Singleton

  // Named pair constructor
  object P extends Dynamic:
    transparent inline def applyDynamicNamed[LA <: LabelType, A, LB <: LabelType, B, T <: Tuple](inline s: "apply")(inline a: (LA, A), b: (LB, B)): Any =
      (a._2, b._2).asInstanceOf[NamedPair[LA, A, LB, B]]


  opaque type NamedPair[LabelA <: LabelType, A, LabelB <: LabelType, B] <: Dynamic = (A, B) & Dynamic

  extension [LabelA <: LabelType, A, LabelB <: LabelType, B](p: NamedPair[LabelA, A, LabelB, B])
    transparent inline def selectDynamic(inline s: String) =
      inline s match
        case _ : LabelA => p._1
        case _ : LabelB => p._2
        case _ =>
          compiletime.error("Field does not exist")


  given [LabelA <: LabelType, A, LabelB <: LabelType, B]: Conversion[(A, B), NamedPair[LabelA, A, LabelB, B]] = identity

  @main def testPair: Unit =
    val john = P(age = 42, name = "John")

    println(john.age)
    println(john.name)

    //john.address // error: Field does not exist

    type Person = NamedPair["age", Int, "name", String]

    val bill: Person = (3, "Bill")

    val john2: Person = john
