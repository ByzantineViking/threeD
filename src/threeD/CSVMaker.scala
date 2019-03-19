package threeD

import java.io.{BufferedWriter, FileWriter}
import scala.collection.JavaConversions._

object Writer {
  def hallway(x: Double, z: Double, y: Double) = {
    val rowOne = Array(x,z,y,x,z+1,y,x,y+1,z)
    val rowTwo = Array(x,z+1,y+1,x,z+1,y,y,x,y+1,z)
    val txt = rowOne.mkString(";") + "\n" + rowTwo.mkString(";")
    
    val file = new FileWriter("/threeD/test.csv")
    file.write(txt)
    file.close()
    
  }
}