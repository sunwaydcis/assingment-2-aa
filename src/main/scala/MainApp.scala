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
      profitMargin = parts(23).toDouble,
      visitors = parts(11).toInt // No. of People column
    )
  }

  // Convert percentage discount (%) to decimal
  def parseDiscount(discount: String): Double = {
    discount.replace("%", "").toDouble / 100
  }

  // Calculate effective price paid by customer
  def effectivePrice(b: Booking): Double = {
    val discountRate = parseDiscount(b.discount)
    b.bookingPrice * (1 - discountRate) * (1 - b.profitMargin)
  }

  def main(args: Array[String]): Unit = {

    val filename = "Hotel_Dataset.csv"
    val source = Source.fromFile(filename, "ISO-8859-1")


    val dataLines = source.getLines().drop(1)

    val bookings = dataLines.map(parseBooking).toList

    println(s"Total bookings loaded: ${bookings.size}")

    // Question 1 Answer: Country with highest number of bookings

    val bookingsByCountry = bookings.groupBy(_.originCountry)

    val countryCounts = bookingsByCountry.map {
      case (country, bookingList) => (country, bookingList.size)
    }
    val topCountry = countryCounts.maxBy(_._2)

    println(
      s"Country with highest number of bookings: ${topCountry._1} (${topCountry._2} bookings)"
    )

    // Question 2 Answer: Hotel that offers most economical option to customers

    val hotelAverages = bookings
      .groupBy(_.hotelName)
      .map { case (hotel, hotelBookings) =>
        val avgPrice =
          hotelBookings.map(effectivePrice).sum / hotelBookings.size
        (hotel, avgPrice)
      }

    val mostEconomicalHotel = hotelAverages.minBy(_._2)

    println(
      s"Most economical hotel (on average): ${mostEconomicalHotel._1} " +
        s"with an average effective price of SGD ${mostEconomicalHotel._2}"
    )

    source.close()
  }
}
