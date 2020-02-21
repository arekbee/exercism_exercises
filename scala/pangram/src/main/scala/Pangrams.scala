object Pangrams {
  val chars = (97 to 122).map(_.toChar).toSet
  def isPangram(input: String): Boolean = {
    (chars & input.toLowerCase.toSet).size == chars.size
  }
}

