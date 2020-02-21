object BeerSong {
    def songFormatN = "%s bottles of beer on the wall, %s bottles of beer.\nTake one down and pass it around, %s bottles of beer on the wall.\n"
    def songFormat2 = "2 bottles of beer on the wall, 2 bottles of beer.\nTake one down and pass it around, 1 bottle of beer on the wall.\n"
    def songFormat1 = "1 bottle of beer on the wall, 1 bottle of beer.\nTake it down and pass it around, no more bottles of beer on the wall.\n"
    def songFormat0 = "No more bottles of beer on the wall, no more bottles of beer.\nGo to the store and buy some more, 99 bottles of beer on the wall.\n"
    
    def recite(nStart :Int, nTake:Int) :String = {
        (nStart to 0 by -1).take(nTake).map{
                case n if n > 2 => songFormatN.format(n,n, n-1)
                case 2 => songFormat2
                case 1 => songFormat1
                case 0 => songFormat0
        }.mkString("\n")
    }
}