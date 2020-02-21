import java.time.{DayOfWeek, LocalDate}
import scala.util.{Try, Success, Failure}
import Schedule.Schedule

case class Meetup(month: Int, year: Int) {
  import Schedule._
  def day(dayOfWeek: Int, schedule: Schedule): LocalDate = {
        val nrs = schedule match {
          case Teenth => 13 to 19
          case First => 1 to 7
          case Second => 8 to 14
          case Third => 15 to 21
          case Fourth => 22 to 31
          case Last => 31 to 21 by -1
        }
      nrs.map(x=>Try(LocalDate.of(year, month, x))).collect{case Success(v)=>v}.filter(_.getDayOfWeek.getValue == dayOfWeek).head
  }
}

object Schedule extends Enumeration {
  type Schedule = Value
  val Teenth, First, Second, Third, Fourth, Last = Value
}

object Meetup {
  val Mon = DayOfWeek.MONDAY.getValue
  val Tue = DayOfWeek.TUESDAY.getValue
  val Wed = DayOfWeek.WEDNESDAY.getValue
  val Thu = DayOfWeek.THURSDAY.getValue
  val Fri = DayOfWeek.FRIDAY.getValue
  val Sat = DayOfWeek.SATURDAY.getValue
  val Sun = DayOfWeek.SUNDAY.getValue
}
