package threeD
import scala.io.Source
import java.io.{FileNotFoundException, IOException}
import java.lang.NumberFormatException
import scala.collection.mutable.Buffer
class GameEngine(sourceFile: String) {
  def readCSV = {
    try {
      val csv = Source.fromFile("/3d_project/src/threeD/walls.csv", "UTF-8").getLines.toArray
      val coordinates = Buffer[Array[Int]]()
      for (line <- csv) {
        line.split(';').map(_.toInt)
      }
      coordinates
    } catch {
      case e: FileNotFoundException => println("Couldn't find that file.")
      case e: IOException => println("Got an IOException!")
      case e: NumberFormatException => println("Does not contain only valid coordinates")
    }
    
  }
}