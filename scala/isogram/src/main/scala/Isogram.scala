object Isogram {
    def isIsogram(str :String) :Boolean = {
        def isIsogramRec(str : List[Char], set:Set[Char]) :Boolean = {
            str match {
                case Nil => true
                case s::tail if set.contains(s.toLower) => false
                case s::tail => isIsogramRec(tail, set + s.toLower)
            }
        }
        isIsogramRec(str.toList.filter(_.isLetterOrDigit), Set.empty[Char])
    }
}