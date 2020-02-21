import scala.collection.mutable
object CollatzConjecture {
    val mem  = mutable.Map(1->0, 2->1, 4->2, 8->3, 16->4, 32->5)
    def getNextVal(n:Int) = {
        n match {
            case n if n % 2 == 0 => n / 2
            case n => 3 * n +1
        }
    }

    def steps(n:Int) : Option[Int] = {
        n match {
            case x if mem.contains(n) => mem.get(n)
            case x if x > 1 => {
                val nn = getNextVal(x)
                val res = 1 + steps(nn).get
                mem.put(x, res)
                Some(res)
            }
            case _ => None
        }
    }
}