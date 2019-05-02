package threeD

import java.nio.file.Path
import java.nio.file.Paths
import java.io.{BufferedWriter, FileWriter}
import scala.collection.JavaConversions._




/** Used to create the world. Always overwrites the chosen file, so if you want to save your creation,
 *  remember to change the "fileName" in ScalaFXApp after creating the world, such that the next time you
 *  launch it will write to different file.
 *  
 *  Creating the world happens at the bottom of this page.
 *  Happy building.
 */
object Writer {
  // All the triangles MUST be created in clock-wise order.
  // Starting points are from looking at it from the mentioned direction, the bottom-left corner, where the bottom and top triangle meet.
  
  def singleLowRightTriangleSouth(x: Double, z: Double, y: Double, width: Double, height: Double, depth: Double): String = {
    // Coordinates of bottom-left corner, so the lowest x and lowest z
    val lowTriangle =   Array(x,z,y,
                              x+width,z+height,y+depth,
                              x+width,z,y).mkString(";") + "\n"
    val backLowTriangle =   Array(x,z,y,
                              x+width,z,y,
                              x+width,z+height,y+depth
                              ).mkString(";") + "\n"
    lowTriangle + backLowTriangle
  }
  def singleLowLeftTriangleSouth(x: Double, z: Double, y: Double, width: Double, height: Double, depth: Double): String = {
    // Coordinates of bottom-left corner, so the lowest x and lowest z
    val lowTriangle =   Array(x,z,y,
                              x-width,z+height,y+depth,
                              x-width,z,y).mkString(";") + "\n"
    val lowBackTriangle =   Array(x,z,y,
                              x-width,z,y,
                              x-width,z+height,y+depth).mkString(";") + "\n"
    lowTriangle + lowBackTriangle
  }
  
  def createCustomSouthFacingWall(x: Double, z: Double, y: Double, width: Double, height: Double, depth: Double): String = {
    // Coordinates of bottom-left corner, so the lowest x and lowest z
    val lowTriangle =   Array(x,z,y,
                              x+width,z+height,y+depth,
                              x+width,z,y).mkString(";")
    val highTriangle =  Array(x,z,y,
                              x,z+height,y+depth,
                              x+width,z+height,y+depth).mkString(";")
    val backLowTriangle =   Array(x,z,y,
                              x+width,z,y,
                              x+width,z+height,y+depth).mkString(";")
    val backHighTriangle =  Array(x,z,y,
                              x+width,z+height,y+depth,
                              x,z+height,y+depth).mkString(";")                          
    val wall: String = lowTriangle + "\n"  + highTriangle + "\n" + backLowTriangle + "\n"  + backHighTriangle + "\n"
    wall
  }
  
  def window(x: Double, z: Double, y: Double, width: Double, height: Double, depth: Double): String = {
    createCustomSouthFacingWall(x,z,y,width,height/4,depth) +  createCustomSouthFacingWall(x,z+ height/4*3,y,width,height/4,depth) +
    createCustomSouthFacingWall(x,z+height/4,y,width/4,height/2,depth) + createCustomSouthFacingWall(x + width/4*3,z+height/4,y,width/4,height/2,depth)
  }
  
  /**
   * Create a new wall that is only seen from South, with the coordinates being the bottom-left corner, so the lowest x and lowest z.
   * @param x-coordinate If looking North, right is positive x and left is negative.
   * @param z-coordinate If looking North, up is positive z and down is negative.
   * @param y-coordinate If looking North, forward is positive y and backward is negative.
   */
  def createSouthFacingWall(x: Double, z: Double, y: Double): String = {
    // Coordinates of bottom-left corner, so the lowest x and lowest z
    val lowTriangle =   Array(x,z,y,
                              x+1,z+1,y,
                              x+1,z,y).mkString(";")
    val highTriangle =  Array(x,z,y,
                              x,z+1,y,
                              x+1,z+1,y).mkString(";")
    val wall: String = lowTriangle + "\n" + highTriangle + "\n"
    wall
  }
  /**
   * Create a new wall that is only seen from North, with the coordinates being the bottom-left corner,  so the highest x and lowest z.
   * @param x-coordinate If looking North, right is positive x and left is negative.
   * @param z-coordinate If looking North, up is positive z and down is negative.
   * @param y-coordinate If looking North, forward is positive y and backward is negative.
   */
  def createNorthFacingWall(x: Double, z: Double, y: Double): String = {
     // Coordinates of bottom-left corner, so the highest x and lowest z
    val lowTriangle =   Array(x,z,y,
                              x-1,z+1,y,
                              x-1,z,y).mkString(";")
    val highTriangle =  Array(x,z,y,
                              x,z+1,y,
                              x-1,z+1,y).mkString(";")
    val wall: String = lowTriangle + "\n" + highTriangle + "\n"
    wall
  }
  /**
   * Create a new wall that is only seen from West, with the coordinates being the bottom-left corner, so the the highest y and lowest z
   * @param x-coordinate If looking North, right is positive x and left is negative.
   * @param z-coordinate If looking North, up is positive z and down is negative.
   * @param y-coordinate If looking North, forward is positive y and backward is negative.
   */
  def createWestFacingWall(x: Double, z: Double, y: Double): String = {
    // Coordinates of bottom-left corner, so the highest y and lowest z
    val lowTriangle  =  Array(x,z,y,
                              x,z+1,y-1,
                              x,z,y-1).mkString(";")
    val highTriangle = Array(x,z,y,
                             x,z+1,y,
                             x,z+1,y-1).mkString(";")
    
    val wall: String = lowTriangle + "\n" + highTriangle + "\n"
    wall
  }
  /**
   *  Create a new wall that is only seen from East, with the coordinates being the bottom-left corner, so the lowest y and lowest z
   * @param x-coordinate If looking North, right is positive x and left is negative.
   * @param z-coordinate If looking North, up is positive z and down is negative.
   * @param y-coordinate If looking North, forward is positive y and backward is negative.
   */
  def createEastFacingWall(x: Double, z: Double, y: Double): String = {
    // Coordinates of bottom-left corner, so the lowest y and lowest z
    val lowTriangle  =  Array(x,z,y,
                              x,z+1,y+1,
                              x,z,y+1).mkString(";")
    val highTriangle = Array(x,z,y,
                             x,z+1,y,
                             x,z+1,y+1).mkString(";")
    
    val wall: String = lowTriangle + "\n" + highTriangle + "\n"
    wall
  }
  /**
   *  Create a new wall that is only seen from up, with the coordinates being the bottom-left corner, so the lowest x and lowest y
   * @param x-coordinate If looking North, right is positive x and left is negative.
   * @param z-coordinate If looking North, up is positive z and down is negative.
   * @param y-coordinate If looking North, forward is positive y and backward is negative.
   */
  def createSkyFacingWall(x: Double, z: Double, y: Double): String = {
    // Coordinates of bottom-left corner, so the lowest x and lowest y
    val lowTriangle =  Array(x,z,y,
                             x+1,z,y+1,
                             x+1,z,y).mkString(";")
    val highTriangle =   Array(x,z,y,
                               x,z,y+1,
                               x+1,z,y+1).mkString(";")
    
    val wall: String = lowTriangle + "\n" + highTriangle + "\n"
    wall
  }
  /**
   *  Create a new wall that is only seen from down, with the coordinates being the bottom-left corner, so the lowest x and highest y
   * @param x-coordinate If looking North, right is positive x and left is negative.
   * @param z-coordinate If looking North, up is positive z and down is negative.
   * @param y-coordinate If looking North, forward is positive y and backward is negative.
   */
  def createGroundFacingWall(x: Double, z: Double, y: Double): String = {
    // Coordinates of bottom-left corner, so the lowest x and highest y
    val lowTriangle =  Array(x,z,y,
                             x+1,z,y-1,
                             x+1,z,y).mkString(";")
    val highTriangle =   Array(x,z,y,
                               x,z,y-1,
                               x+1,z,y-1).mkString(";")
    
    val wall: String = lowTriangle + "\n" + highTriangle + "\n"
    wall
  }
  
  
  def customHor(x: Double, z: Double, y: Double, width: Double, height: Double, depth: Double ): String = {
    // Coordinates of bottom-left corner, so the lowest x and highest y
    val lowTriangle =  Array(x,z,y,
                             x+width,z+height,y-depth,
                             x+width,z,y).mkString(";")
    val highTriangle =   Array(x,z,y,
                               x,z+height,y-depth,
                               x+width,z+height,y-depth).mkString(";")
    val backLowTriangle =  Array(x,z,y,
                                x+width,z,y,
                                x+width,z+height,y-depth
                             ).mkString(";")
    val backHighTriangle =   Array(x,z,y,
                               x+width,z+height,y-depth,
                               x,z+height,y-depth
                               ).mkString(";")
    val wall: String = lowTriangle + "\n"  + highTriangle + "\n" + backLowTriangle + "\n"  + backHighTriangle + "\n"
    wall
  }
  
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
  
  def roofLeft(x: Double, z: Double, y: Double,depth: Double,  height: Double, width: Double ): String = {
    // Coordinates of downleft corner, so the lowest y, lowest x, and lowest z
    val lowTriangle =   Array(x,z,y,
                              x+depth,z+height,y-width,
                              x,z,y-width).mkString(";")
    val highTriangle =  Array(x,z,y,
                              x+depth,z+height,y,
                              x+depth,z+height,y-width).mkString(";")
    val backLowTriangle =   Array(x,z,y,
                              x,z,y-width,
                              x+depth,z+height,y-width).mkString(";")
    val backHighTriangle =  Array(x,z,y,
                              x+depth,z+height,y-width,
                              x+depth,z+height,y).mkString(";")                      
    val roof: String = lowTriangle + "\n"  + highTriangle + "\n" + backLowTriangle + "\n"  + backHighTriangle + "\n"
    roof
  }
  def roofRight(x: Double, z: Double, y: Double,depth: Double,  height: Double, width: Double ): String = {
    // Coordinates of downleft corner, so the lowest y, lowest x, and lowest z
    val lowTriangle =   Array(x,z,y,
                              x-depth,z+height,y+width,
                              x,z,y+width).mkString(";")
    val highTriangle =  Array(x,z,y,
                              x-depth,z+height,y,
                              x-depth,z+height,y+width).mkString(";")
    val backLowTriangle =   Array(x,z,y,
                              x,z,y+width,
                              x-depth,z+height,y+width
                              ).mkString(";")
    val backHighTriangle =  Array(x,z,y,
                              x-depth,z+height,y+width,
                              x-depth,z+height,y
                              ).mkString(";")
    val roof: String = lowTriangle + "\n"  + highTriangle + "\n" + backLowTriangle + "\n"  + backHighTriangle + "\n"
    roof
  }
  // The coordinates of the highest x and lowest y and lowest z. So if looking North, the bottom right front corner.
  def block(x: Double, z: Double, y: Double): String = {
    createSouthFacingWall(x-1,z,y) + createNorthFacingWall(x,z,y+1) + createWestFacingWall(x-1,z,y+1) + createEastFacingWall(x,z,y) + createSkyFacingWall(x-1,z+1,y) + createGroundFacingWall(x-1,z,y+1) 
  }
  
  def openBox(x: Double, z: Double, y: Double): String = {
    createNorthFacingWall(x,z,y+1) + createWestFacingWall(x-1,z,y+1) + createEastFacingWall(x,z,y) + createSkyFacingWall(x-1,z+1,y) + createGroundFacingWall(x-1,z,y+1) +
    createSouthFacingWall(x-1,z,y+1) + createEastFacingWall(x-1,z,y) + createWestFacingWall(x,z,y+1) + createGroundFacingWall(x-1,z+1,y+1) + createSkyFacingWall(x-1,z,y) 
  }
  
  def hallwayNorth(x: Double, z: Double, y: Double, length: Int): String = {
    var ret : String = ""
    //So if I'm standing in the middle of crossroads and looking at y,
    //I look slightly to the left to see a corner (closest corner of the block)
    //Coordinates are the bottom coordinates of that corner (where it hits the ground)
    for (i <- 0 until length) {
      ret = ret + block(x, z, y+i) + block(x + 2, z, y+i)
    }
    ret
  }
  def wideHallwayNorth(x: Double, z: Double, y: Double, length: Int): String = {
    var ret : String = ""
    //So if I'm standing in the middle of crossroads and looking at y,
    //I look slightly to the left to see a corner (closest corner of the block)
    //Coordinates are the bottom coordinates of that corner (where it hits the ground)
    for (i <- 0 until length) {
      ret = ret + block(x, z, y+i) + block(x + 3, z, y+i)
    }
    ret
  }
  
  def magnificentStairs(x: Double, z: Double, y: Double): String = {
    block(x+4.5,z-0.5,y) + 
    block(x+3.5,z+0.5,y+1) +
    block(x+2.5,z+1.5,y+2) + 
    block(x+1.5,z+2.5,y+3) + 
    block(x-4.5,z-0.5,y) + 
    block(x-3.5,z+0.5,y+1) +
    block(x-2.5,z+1.5,y+2) + 
    block(x-1.5,z+2.5,y+3) +
    slope(x+0.5,z-0.5,y) +
    slope(x+1.5,z-0.5,y) +
    slope(x+2.5,z-0.5,y) +
    slope(x-0.5,z-0.5,y) +
    slope(x-1.5,z-0.5,y) +
    slope(x-2.5,z-0.5,y) +
    slope(x-3.5,z-0.5,y) +
    slope(x-4.5,z-0.5,y) +
    slope(x+0.5,z+0.5,y+1) +
    slope(x+1.5,z+0.5,y+1) +
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
    
//----------------------------------------------------------------------------------------------//
    //---------------------------//
    //----- MAKING THE WORLD-----//
    //---------------------------//
    
    
    // Here you can use any of the above methods or combine/ create new ones.
    // 
    // hint : check which coordinate is given in the documentation of the method.
    
    
    // Misc
//    file.write(block(-0.5,-0.5, 3))
//    file.write(block(7.5,-0.5, 10))
//    file.write(block(-9.5,-0.5, 10))
//    file.write(block(3.5,3.5, 4))
//    file.write(openBox(2.5,-0.5,2.0))
    
    
    
    // Jumping blocks.
//    file.write(block(-1.5, -0.5, 2))
//    file.write(block(-0.5, 0.5, 3))
//    file.write(block(0.5, 1.5, 2))
//    file.write(block(-0.5, 2.5, 1))
    
    // A grand staircase.
//    file.write(this.magnificentStairs(0.5, -0.5, 3))
    
//     A barn-like object.
    file.write(hallwayNorth(-0.5,-0.5,4, 2))
    file.write(hallwayNorth(-0.5,0.5,4, 2))
    file.write(roofLeft(-1.5, 1.5, 5, 1.5, 3,1))
    file.write(roofLeft(-1.5, 1.5, 6, 1.5, 3,1))
    file.write(roofRight(1.5, 1.5, 4, 1.5, 3,1))
    file.write(roofRight(1.5, 1.5, 5, 1.5, 3,1))
    file.write(customHor(-0.5, 1.5, 6, 1, 0, 2))
    file.write(singleLowRightTriangleSouth(-1.5,1.5,4, 1.25, 2.50, 0))
    file.write(singleLowLeftTriangleSouth(1.5,1.5,4,1.25,2.50,0))
    file.write(createCustomSouthFacingWall(-0.25,1.5,4,0.5, 0.25, 0))
    file.write(createCustomSouthFacingWall(-0.25,3.5,4,0.5, 0.25, 0))
    file.write(createCustomSouthFacingWall(-0.25,3.0,4,0.5, 0.25, 0))
    
    file.write(createCustomSouthFacingWall(-0.5,-0.5,4,0.25, 2, 0))
    file.write(createCustomSouthFacingWall(0.25,-0.5,4,0.25, 2, 0))
    
    file.write(window(-0.5,-0.5,6, 1,2,0))
    file.write(window(-0.25,1.0,4, 0.5,0.5,0))
    
    
    
    
    
    
    
    file.close()
    
}