import java.time.LocalDate
import java.time.LocalDateTime

object Gigasecond {
  val gigasecond = 1e9.toLong
  def add(startDate: LocalDate): LocalDateTime = add(startDate.atStartOfDay)

  def add(startDateTime: LocalDateTime): LocalDateTime = startDateTime.plusSeconds(gigasecond)
}
