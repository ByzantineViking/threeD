package threeD

import java.nio.file.Path
import java.nio.file.Paths
import java.io.{BufferedWriter, FileWriter}
import scala.collection.JavaConversions._

object Writer {
  def createXWall(x: Double, z: Double, y: Double): String = {
    // Coordinates of downright corner, so the highest x and lowest z
    val lowTriangle =   Array(x    ,z    ,y    ,
                              x    ,z+1  ,y    ,
                              x-1  ,z    ,y     ).mkString(";")
    val highTriangle =  Array(x-1  ,z+1  ,y    ,
                              x    ,z+1  ,y    ,
                              x-1  ,z    ,y     ).mkString(";")
    val wall: String = lowTriangle + "\n" + highTriangle + "\n"
    wall
  }
  def createYWall(x: Double, z: Double, y: Double): String = {
    // Coordinates of downleft corner, so the lowest y and lowest z
    val lowTriangle =   Array(x    ,z    ,y    ,
                              x    ,z+1  ,y    ,
                              x    ,z    ,y+1   ).mkString(";")
    val highTriangle =  Array(x    ,z+1  ,y    ,
                              x    ,z+1  ,y+1  ,
                              x    ,z    ,y+1   ).mkString(";")
    val wall: String = lowTriangle + "\n" + highTriangle + "\n"
    wall
  }
  
  
  // Slope doesn't work in the draw order function.
  def slope(x: Double, z: Double, y: Double): String = {
    // Coordinates of downleft corner, so the lowest y and lowest dwwz
    val lowTriangle =   Array(x    ,z    ,y    ,
                              x    ,z+1  ,y+1  ,
                              x-1  ,z    ,y    ).mkString(";")
    val highTriangle =  Array(x-1  ,z    ,y    ,
                              x-1  ,z+1  ,y+1  ,
                              x    ,z+1  ,y+1   ).mkString(";")
    val slope: String = lowTriangle + "\n"  + highTriangle + "\n"
    slope
  }
  
  
  def block(x: Double, z: Double, y: Double): String = {
    createXWall(x,z,y) + createYWall(x,z,y) + createXWall(x,z,y+1) + createYWall(x-1,z,y)
  }
  
  def crossroads(x: Double, z: Double, y: Double): String = {
    //So if I'm standing in the middle of crossroads and looking at y,
    //I look slightly to the left to see a corner (closest corner of the block)
    //Coordinates are the bottom coordinates of that corner (where it hits the ground)
    block(x, z, y) + block(x + 2, z, y) //+ block(x, z, y - 1) + block(x + 2, z, y -1)
  }
    //Current Dir
    val currentDir = Paths.get(".").toAbsolutePath().normalize().toString()
    val filePath = currentDir + "/src/resources/data.csv"
    val file = new FileWriter(filePath)
    
    //Making the world
    file.write(crossroads(-0.5,-0.5,1))
    file.write(slope(0.5, -0.5, 1))
    
    
    file.close()
    
}