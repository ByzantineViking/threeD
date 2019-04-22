package threeD

import scala.io.Source
import java.io.{FileNotFoundException, IOException}
import java.lang.NumberFormatException
import scala.collection.mutable.Buffer
import java.nio.file.Path
import java.nio.file.Paths
import java.io.{BufferedWriter, FileWriter}
import scala.collection.JavaConversions._

/**
 * Reads object files
 */
object ObjectReader {
  
  def readObj: Array[Array[Double]] = {
    try { 
      val obj = Source.fromFile(this.filePath).getLines().toArray.drop(2)
      val coordinates = Buffer[Array[Double]]()
      val vertecies = Buffer[Array[Double]]()
      for (line <- obj) {
        if (line.isEmpty()) {
          //Skip
        } else {
          if (line.head == 'v') {
            vertecies += line.drop(1).trim.split(' ').map(x => x.toDouble)
          } 
        } 
      }
      for (line <- obj) {
        if(line.isEmpty()) {
          // Skip
        } else {
          if (line.head == 'f') {
            val triangleTrio = line.drop(1).trim.split(' ').map(c => vertecies(c.toInt - 1))
            for(z <- triangleTrio) {
              coordinates.append(z)
            }
          } else {
            
          }
        }
      }
      coordinates.toArray
      
    } catch {
      case e: FileNotFoundException => println("Couldn't find the file with the file path: " +  this.filePath); System.exit(1) ; Array(Array(-1.0))
      case e: IOException => println("Got an IOException!"); System.exit(1) ; Array(Array(-1.0))
      case e: NumberFormatException => println("Does not contain only valid coordinates"); System.exit(1) ; Array(Array(-1.0))
    }
  }
  
  def filePath: String = {
    //Finding the file
      val currentDir = Paths.get(".").toAbsolutePath().normalize().toString()
      val resourcesDir = currentDir + "/src/resources/"
      resourcesDir + Front.objectName
  }
    
}