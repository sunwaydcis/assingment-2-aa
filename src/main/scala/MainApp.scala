import scala.io.Source

object MainApp {

  def parseBooking(line: String): Booking = {
    val parts = line.split(",")

    Booking(
      bookingId = parts(0),
      customerId = parts(3),
      gender = parts(4),
      age = parts(5).toInt,
      originCountry = parts(6),
      destinationCountry = parts(9),
      hotelName = parts(16),
      bookingPrice = parts(20).toDouble,
      discount = parts(21),
      gst = parts(22).toDouble,
      profitMargin = parts(23).toDouble
    )
  }

  def main(args: Array[String]): Unit = {

    val filename = "Hotel_Dataset.csv"
    val source = Source.fromFile(filename)

    val dataLines = source.getLines().drop(1)

    val bookings = dataLines.take(30).map(parseBooking).toList

    println(s"Total bookings loaded: ${bookings.size}")

    val bookingsByCountry = bookings.groupBy(_.originCountry)

    val countryCounts = bookingsByCountry.map {
      case (country, bookingList) => (country, bookingList.size)
    }
    val topCountry = countryCounts.maxBy(_._2)

    println(
      s"Country with highest number of bookings: ${topCountry._1} (${topCountry._2} bookings)"
    )

    source.close()
  }
}
