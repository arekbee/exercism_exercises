object FlattenArray{
    def flatten(l: List[Any]) :List[Any] = {
        l match {
            case (xs1 :List[Any]) :: xs2 =>  flatten(xs1) ++  flatten(xs2)
            case x :: xs if x != null => x :: flatten(xs)
            case null :: xs => flatten(xs)
            case Nil => Nil
        }
    }
}