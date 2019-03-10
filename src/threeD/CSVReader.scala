package threeD
import scala.io.Source
import java.io.{FileNotFoundException, IOException}
import java.lang.NumberFormatException
import scala.collection.mutable.Buffer
object CSVReader {
  def readCSV: Array[Array[Double]] = {
    try { 
      val csv = Source.fromFile("U:/Users/Teemu/Desktop/scales/3d_project/src/threeD/data.csv").getLines().toArray
      val coordinates = Buffer[Array[Double]]()
      for (line <- csv) {
        coordinates += line.split(';').map(x => x.replace(',', '.').toDouble)
      }
      for (a <- coordinates) {
        for (b<- a) {
          println(b)
        }
      }
      coordinates.toArray
    } catch {
      case e: FileNotFoundException => println("Couldn't find that file.") ; Array(Array(-1.0))
      case e: IOException => println("Got an IOException!"); Array(Array(-1.0))
      case e: NumberFormatException => println("Does not contain only valid coordinates"); Array(Array(-1.0))
    }
    
  }
}