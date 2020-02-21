object Allergies {
    import Allergen._
    def allergicTo( allergen:Allergen, nr:Int) :Boolean = (nr & allergen.id) != 0

    def list(nr:Int) :List[Allergen] = Allergen.All.filter(allergicTo(_,nr)).toList
}


object Allergen extends Enumeration {
  type Allergen = Value
    val Eggs = Value(1, "Eggs")
    val Peanuts = Value(2, "Peanuts")
    val Shellfish = Value(4, "Shellfish")
    val Strawberries = Value(8, "Strawberries")
    val Tomatoes = Value(16, "Tomatoes")
    val Chocolate = Value(32, "Chocolate")
    val Pollen = Value(64, "Pollen")
    val Cats = Value(128, "Cats")

    val All = Seq(Eggs, Peanuts, Shellfish,Strawberries,Tomatoes,Chocolate,  Pollen, Cats  )
}
