object HighScores {
    def latest( l:List[Int]) :Int = l.last
    def personalBest ( l:List[Int]) :Int = l.max
    def personalTop ( l:List[Int]) :List[Int]  = l.sorted.reverse.take(3)

    def report ( l:List[Int]) :String  ={
            (personalBest(l), latest(l)) match {
                case (b,l) if b > l => s"Your latest score was $l. That's ${b-l} short of your personal best!"
                case (_,l) =>  s"Your latest score was $l. That's your personal best!"
            }
    }
}