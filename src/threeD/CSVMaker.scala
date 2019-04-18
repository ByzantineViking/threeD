package threeD

import java.nio.file.Path
import java.nio.file.Paths
import java.io.{BufferedWriter, FileWriter}
import scala.collection.JavaConversions._

object Writer {
  
  // All the triangles MUST be created in clock-wise order.
  def createXWall(x: Double, z: Double, y: Double): String = {
    // Coordinates of downright corner, so the highest x and lowest z
    val lowTriangle =   Array(x,z,y,
                              x-1,z,y,
                              x,z+1,y).mkString(";")
    val highTriangle =  Array(x-1,z,y,
                              x-1,z+1,y,
                              x,z+1,y).mkString(";")
    val wall: String = lowTriangle + "\n" + highTriangle + "\n"
    wall
  }
  def createYWall(x: Double, z: Double, y: Double): String = {
    // Coordinates of downright corner, so the lowest y and lowest z
    val highTriangle = Array(x,z,y+1,
                             x,z+1,y+1,
                             x,z+1,y).mkString(";")
    val lowTriangle  =  Array(x,z,y,
                              x,z,y+1,
                              x,z+1,y).mkString(";")
    val wall: String = lowTriangle + "\n" + highTriangle + "\n"
    wall
  }
  def createHorizontalWall(x: Double, z: Double, y: Double): String = {
    // Coordinates of downleft corner, so the lowest x and lowest y.
    val highTriangle =   Array(x,z,y,
                               x,z,y+1,
                               x+1,z,y+1).mkString(";")
    val lowTriangle =  Array(x,z,y,
                             x+1,z,y+1,
                             x+1,z,y).mkString(";")
    val wall: String = lowTriangle + "\n" + highTriangle + "\n"
    wall
  }
  
  
  // Slope doesn't work in the draw order function.
  def slope(x: Double, z: Double, y: Double): String = {
    // Coordinates of downleft corner, so the lowest y, lowest x, and lowest z
    val lowTriangle =   Array(x,z,y,
                              x+1,z+1,y+1,
                              x+1,z,y).mkString(";")
    val highTriangle =  Array(x,z,y,
                              x,z+1,y+1,
                              x+1,z+1,y+1).mkString(";")
    val slope: String = lowTriangle + "\n"  + highTriangle + "\n"
    slope
  }
  
  
  def block(x: Double, z: Double, y: Double): String = {
    createXWall(x,z,y) + createYWall(x,z,y) + createXWall(x,z,y+1) + createYWall(x-1,z,y) + createHorizontalWall(x-1,z+1,y) +createHorizontalWall(x-1,z,y)
  }
  
  def hallwayNorth(x: Double, z: Double, y: Double): String = {
    //So if I'm standing in the middle of crossroads and looking at y,
    //I look slightly to the left to see a corner (closest corner of the block)
    //Coordinates are the bottom coordinates of that corner (where it hits the ground)
    block(x, z, y) + block(x + 2, z, y) //+ block(x, z, y - 1) + block(x + 2, z, y -1)
  }
  
  def magnificentStairs(x: Double, z: Double, y: Double): String = {
    block(x+4.5,z-0.5,y) + 
    block(x+3.5,z+0.5,y+1) +
    block(x+2.4,z+1.5,y+2) + 
    block(x+1.5,z+2.5,y+3) + 
    block(x-4.5,z-0.5,y) + 
    block(x-3.5,z+0.5,y+1) +
    block(x-2.4,z+1.5,y+2) + 
    block(x-1.5,z+2.5,y+3) +
    slope(x+0.5,z-0.5,y) +
    slope(x+1.5,z-0.5,y) +
    slope(x+2.5,z-0.5,y) +
    slope(x+3.5,z-0.5,y) +
    slope(x-0.5,z-0.5,y) +
    slope(x-1.5,z-0.5,y) +
    slope(x-2.5,z-0.5,y) +
    slope(x-3.5,z-0.5,y) +
    slope(x-4.5,z-0.5,y) +
    slope(x+0.5,z+0.5,y+1) +
    slope(x+1.5,z+0.5,y+1) +
    slope(x-3.5,z+0.5,y+1) +
    slope(x-0.5,z+0.5,y+1) +
    slope(x-1.5,z+0.5,y+1) +
    slope(x-2.5,z+0.5,y+1) +
    slope(x-3.5,z+0.5,y+1) +
    slope(x+0.5,z+1.5,y+2) +
    slope(x-0.5,z+1.5,y+2) +
    slope(x-1.5,z+1.5,y+2) +
    slope(x-2.5,z+1.5,y+2) +
    slope(x-0.5,z+2.5,y+3) +
    slope(x-1.5,z+2.5,y+3)
  }
    //Current Dir
    val currentDir = Paths.get(".").toAbsolutePath().normalize().toString()
    val filePath = currentDir + "/src/resources/data.csv"
    val file = new FileWriter(filePath)
    
    //Making the world
  
    file.write(magnificentStairs(0.5,0,1))
    
    
    file.close()
    
}