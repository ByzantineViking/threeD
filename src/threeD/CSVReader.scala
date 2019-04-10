package threeD
import scala.io.Source
import java.io.{FileNotFoundException, IOException}
import java.lang.NumberFormatException
import scala.collection.mutable.Buffer
import java.nio.file.Path
import java.nio.file.Paths
object CSVReader {
  def readCSV: Array[Array[Double]] = {
    try { 
      //Finding the file
      val currentDir = Paths.get(".").toAbsolutePath().normalize().toString()
      val filePath = currentDir + "/src/resources/"
      val file = filePath + "data.csv"
      val csv = Source.fromFile(file).getLines().toArray
      val coordinates = Buffer[Array[Double]]()
      for (line <- csv) {
        coordinates += line.split(';').map(x => x.replace(',', '.').toDouble)
      }
//      for (a <- coordinates) {
//        for (b<- a) {
//          println(b)
//        }
//      }
      coordinates.toArray
    } catch {
      case e: FileNotFoundException => println("Couldn't find that file.") ; Array(Array(-1.0))
      case e: IOException => println("Got an IOException!"); Array(Array(-1.0))
      case e: NumberFormatException => println("Does not contain only valid coordinates"); Array(Array(-1.0))
    }
    
  }
}