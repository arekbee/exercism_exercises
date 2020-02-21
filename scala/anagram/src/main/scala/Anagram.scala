object Anagram {
    def findAnagrams(str :String, items: List[String]) : List[String] = {
        items.filter(_.length == str.length).filter(matches(str, _)).filterNot(isSame(str,_))
    }

    def matches(strPattrn:String, strToCheck:String) :Boolean = {
        val strPatternMap = strPattrn.toLowerCase.toArray.groupBy(identity).mapValues(_.size)
        val strToCheckMap = strToCheck.toLowerCase.toArray.groupBy(identity).mapValues(_.size)
        strToCheckMap == strPatternMap
    }
    def isSame(strPattrn:String, strToCheck:String) :Boolean ={
        strPattrn.toLowerCase == strToCheck.toLowerCase
    }
}