object Bob {
  def isYelling (str:String) : Boolean = str.matches(".*[a-zA-Z]+.*") && str.toUpperCase() == str

  def response(statement: String): String = {
      val sTrim = statement.trim()
      sTrim match {
        case "" => "Fine. Be that way!"
        case str if str.endsWith("?") && isYelling(str)  => "Calm down, I know what I'm doing!"
        case str if str.endsWith("?") => "Sure."
        case str if isYelling(str)  => "Whoa, chill out!"
        case _ => "Whatever."
      }
  }
}
