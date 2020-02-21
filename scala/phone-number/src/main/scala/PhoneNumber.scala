import PartialFunction._

object PhoneNumber {

    def clean(str :String) :Option[String] = {
        val list0or1 = List('0','1')

        def validMatch(l :List[Char]) :Boolean= {
            cond(l) { case a1::_::_::e1::rest if rest.size ==  6 && !list0or1.contains(a1)  && !list0or1.contains(e1) => true}
        }

        str.toList.filter(_.isDigit) match {
            case str  if validMatch(str) => Some(str.mkString)
            case '1'::str  if validMatch(str) => Some(str.mkString)
            case _ => None
        }
    }
}