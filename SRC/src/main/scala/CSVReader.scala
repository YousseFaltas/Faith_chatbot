import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine
import java.io.FileNotFoundException

object CSVReader {
  private val processedLinks = new ListBuffer[String]()
  private val contexts = List("Make", "Model", "Color", "Price", "Year","Mileage")

  def getResult(keywords: List[String], filename: String = "modified_file-3.csv"): Option[String] = {
    val matchingLines = new ListBuffer[String]()
    var matchesFound = 0

    try {
      val bufferedSource = io.Source.fromFile(filename)
      for (currentLine <- bufferedSource.getLines()) {
        if (currentLine.nonEmpty && matchesFound < 10) { // Stop searching after finding 10 matches
          val cols = currentLine.split(",").map(_.trim)
          if (cols.length > 0) {
            val link = cols(cols.length - 1) // Get the last column (link)
            if (!processedLinks.contains(link)) { // Check if link is not already processed
              val carData = cols.take(cols.length - 1) // Exclude the link column
              var allMatched = true // Flag to check if all contexts are matched
              val matchedContexts = contexts.zipWithIndex.map { case (context, index) =>
                val keyword = if (keywords.lift(index).exists(_.nonEmpty)) keywords.lift(index).getOrElse("") else ""
                if (keyword.nonEmpty) {
                  val columnIndex = context.toLowerCase match {
                    case "year" => cols.indexWhere(_.contains(keyword))
                    case "price" => cols.indexWhere(_.trim == keyword.trim)
                    case "color" => cols.indexWhere(_.toLowerCase.contains(keyword.toLowerCase))
                    case "mileage" => cols.indexWhere(_.trim == keyword.trim)
                    case "make" => cols.indexWhere(_.toLowerCase.contains(keyword.toLowerCase))
                    case "model" => cols.indexWhere(_.toLowerCase.contains(keyword.toLowerCase))
                    case _ => -1 // Default value if context is not recognized
                  }
                  if (columnIndex != -1 && carData(columnIndex).toLowerCase.contains(keyword.toLowerCase))
                    s"$context: ${carData(columnIndex)}"
                  else {
                    allMatched = false
                    ""
                  }
                } else {
                  ""
                }
              }
              if (allMatched) {
                val resultString = matchedContexts.mkString(", ")
                matchingLines += s"${cols.mkString(", ")}, $resultString, Link: $link"
                processedLinks += link
                matchesFound += 1
              }
            }
          }
        }
      }
      bufferedSource.close()

      if (matchingLines.nonEmpty) {
        Some(matchingLines.mkString("\n"))
      } else {
        println(s"No more results found for the provided keywords.")
        None
      }
    } catch {
      case e: FileNotFoundException =>
        println("File not found. Please check the file path and try again.")
        None
      case e: Exception =>
        println("There's no matches to this preferences")
       // println(s"An error occurred: ${e.getMessage}")
        None
      }
    }
}
