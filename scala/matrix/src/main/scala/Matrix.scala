case class Matrix(strmatrix:String) {
    private[this] val rows = strmatrix.split("\n")
    private[this] val firstColumn = rows(0).split(" ")

    val matrix  = Array.ofDim[Int](rows.length, firstColumn.length)
    for(r <- 0 until rows.length) {
        matrix(r) = rows(r).split(" ").map(_.toInt)
    }

    def column(i:Int) :Vector[Int] =  matrix.map(_(i)).toVector
    def row(i:Int) :Vector[Int] = matrix(i).toVector
}