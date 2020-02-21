object Acronym {
  def abbreviate(phrase: String): String = phrase.split(Array(' ', '-')).filterNot(_=="").map(_.head).map(_.toUpper).mkString("")
}
