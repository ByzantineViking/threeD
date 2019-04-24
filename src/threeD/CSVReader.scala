package threeD
import scala.io.Source
import java.io.{FileNotFoundException, IOException}
import java.lang.NumberFormatException
import scala.collection.mutable.Buffer
import java.nio.file.Path
import java.nio.file.Paths
object CSVReader {
  def filePath: String = {
    //Finding the file
      val currentDir = Paths.get(".").toAbsolutePath().normalize().toString()
      val resourcesDir = currentDir + "/src/resources/"
      resourcesDir + Front.fileName
  }
  def readCSV: Array[Array[Double]] = {
    try { 
      val csv = Source.fromFile(this.filePath).getLines().toArray
      val coordinates = Buffer[Array[Double]]()
      for (line <- csv) {
        coordinates += line.split(';').map(x => x.replace(',', '.').toDouble)
      }
      coordinates.toArray
    } catch {
      case e: FileNotFoundException => println("Couldn't find the file with the file path: " +  this.filePath); System.exit(1) ; Array(Array(-1.0))
      case e: IOException => println("Got an IOException!"); System.exit(1) ; Array(Array(-1.0))
      case e: NumberFormatException => println("Does not contain only valid coordinates"); System.exit(1) ; Array(Array(-1.0))
    }
    
  }
  
  
  def fileHelper(name: String) = {
    //Finding the file
      val currentDir = Paths.get(".").toAbsolutePath().normalize().toString()
      val resourcesDir = currentDir + "/src/resources/"
      resourcesDir + name
  }
}