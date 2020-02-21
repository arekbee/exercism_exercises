import math.{max,min}

case class Frame(pins:List[Int], idx:Int) {
    def value = pins.sum
    def valueAll = pins.sum ==10
    def isBase = idx < 10
    def valueOutOfScope = value > 10 || pins.exists(_ < 0 ) || pins.exists(_ > 10 )
    def throws = pins.length
    def has2Throws = throws == 2
    def fst = pins.head
    def snd = if (has2Throws) pins.tail.head else 0 

    def isStrike =  isBase && pins.length == 1 && valueAll
    def isSpare = isBase && !isStrike && has2Throws && valueAll
    def isStrikeOrSpare = isStrike  || isSpare 
}

case class Bowling(frames: List[Frame] = Nil)  {
    def roll(nr :Int) : Bowling = {
        frames match { 
            case Nil => new Bowling(List(Frame(List(nr), 0)))
            case x::xs => 
                if(nr ==10 || x.pins.last==10 || x.has2Throws) 
                    new Bowling( Frame(List(nr), x.idx+1) :: frames)
                else 
                    new Bowling(Frame(x.pins :+nr, x.idx) :: xs )
        } 
    }


    def score() : Either[Int,Int] = {
        val frames2 = frames.reverse
        frames2 match {
            case Nil =>  Left(0)
            case _::_ if frames2.length > 12
                || frames2.exists(_.valueOutOfScope)
                || frames2.length < 9 
                || (frames2.length>11 && frames2(10).fst != 10)
                || (frames2.length>10 && !frames2(9).valueAll)
                || (frames2.length==10 && frames2(9).isStrikeOrSpare)
                || (frames2.length== 11 && frames2(9).isStrike && !frames2(10).has2Throws)
                => Left(0)
            case _::_ =>
                val res = frames2.foldLeft( (0, false,0,0))
                { case ((score, spare, strikeMulti, strikeCounter), frame  ) =>
                    val adBase =  if(frame.isBase ) frame.value else 0 
                    val addSpare = if(spare) frame.fst else 0 
                    
                    val addStrike = if(strikeMulti>0 && strikeCounter>0) frame.fst * strikeMulti  else 0
                    val addStrike2 = if(strikeMulti>0 && strikeCounter>0  && frame.has2Throws) frame.snd   else 0
                    
                    val newstrikeMulti =  if(frame.isStrike) min(strikeMulti+1,2) 
                                            else if(strikeCounter>0) 1 
                                            else max(strikeMulti-1,0)

                    val newstrikeCounter =  if(frame.isStrike) 2 
                                            else if(frame.has2Throws) max(strikeCounter-2,0) 
                                            else max(strikeCounter-1,0)

                    (score + adBase + addStrike + addStrike2 + addSpare, frame.isSpare, newstrikeMulti,newstrikeCounter)                
            }
            Right(res._1)
        }
        
    }
}