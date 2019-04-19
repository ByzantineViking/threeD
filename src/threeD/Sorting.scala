package threeD

import scala.math._

object Sorter {
  
  
  
  
  
  // Sorting the vectors ( 3 vectors to represent each end of triangle) by their summed up distance to the kamera.
  def sort(array: Array[Array[VectorVer]]): Array[Array[VectorVer]] = {
                                         array.sortWith((x, y) => {(
                                         sqrt(pow((x(0).validVector(0)(0) - Front.kamera.pos.validVector(0)(0)), 2))
                                       + sqrt(pow((x(0).validVector(1)(0) - Front.kamera.pos.validVector(1)(0)), 2))
                                       + sqrt(pow((x(0).validVector(2)(0) - Front.kamera.pos.validVector(2)(0)), 2))
                                       
                                       + sqrt(pow((x(1).validVector(0)(0) - Front.kamera.pos.validVector(0)(0)), 2))
                                       + sqrt(pow((x(1).validVector(1)(0) - Front.kamera.pos.validVector(1)(0)), 2))
                                       + sqrt(pow((x(1).validVector(2)(0) - Front.kamera.pos.validVector(2)(0)), 2))
                                       
                                       + sqrt(pow((x(2).validVector(0)(0) - Front.kamera.pos.validVector(0)(0)), 2))
                                       + sqrt(pow((x(2).validVector(1)(0) - Front.kamera.pos.validVector(1)(0)), 2))
                                       + sqrt(pow((x(2).validVector(2)(0) - Front.kamera.pos.validVector(2)(0)), 2))
                                       
                                       >
                                         
                                         sqrt(pow((y(0).validVector(0)(0) - Front.kamera.pos.validVector(0)(0)), 2))
                                       + sqrt(pow((y(0).validVector(1)(0) - Front.kamera.pos.validVector(1)(0)), 2))
                                       + sqrt(pow((y(0).validVector(2)(0) - Front.kamera.pos.validVector(2)(0)), 2))
                                       
                                       + sqrt(pow((y(1).validVector(0)(0) - Front.kamera.pos.validVector(0)(0)), 2))
                                       + sqrt(pow((y(1).validVector(1)(0) - Front.kamera.pos.validVector(1)(0)), 2))
                                       + sqrt(pow((y(1).validVector(2)(0) - Front.kamera.pos.validVector(2)(0)), 2))
                                       
                                       + sqrt(pow((y(2).validVector(0)(0) - Front.kamera.pos.validVector(0)(0)), 2))
                                       + sqrt(pow((y(2).validVector(1)(0) - Front.kamera.pos.validVector(1)(0)), 2))
                                       + sqrt(pow((y(2).validVector(2)(0) - Front.kamera.pos.validVector(2)(0)), 2))
    )}
  )}
  
}