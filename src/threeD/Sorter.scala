package threeD

import scala.math._
import scala.collection.mutable.Buffer
object Sorter {
  
  /**
   * Takes three points, and the kamera, which has information about drawing plane.
   * If a triangle is completely behind kamera, returns no triangles, and if it is completely in front, return the original triangle.
   * If a triangle has 1 point in front of kamera, returns 1 triangle which has the 1 visible point and the two invisible points have been replaced by the intersection points of the sides of the triangle and the viewing plane.
   * IF a triangle has 2 points in front of kamera, this rectangle of the 2 points and two intersection points will be cut into two triangles.
   * 
   */
  def clipping(triangle : Array[VectorVer], planePoint: VectorVer, rot: VectorVer): (Option[Array[VectorVer]],Option[Array[VectorVer]]) = {
        var insidePoints = Buffer[VectorVer]()
        var outsidePoints = Buffer[VectorVer]()
        var intersectionPoints = Buffer[VectorVer]()
         
        MathHelper.intersectPointBetweenPoints(triangle(0), triangle(1), planePoint, rot) match {
          case Some(intersect) => {
//            println("Distance from plane: " + MathHelper.distanceFromPlane(triangle(0), planePoint, rot))
            intersectionPoints.append(intersect)
            if (MathHelper.distanceFromPlane(triangle(0), planePoint, rot) > 0 && MathHelper.distanceFromPlane(triangle(1), planePoint, rot) < 0) {
              insidePoints.append(triangle(0))
//              println("Point 0 : " + triangle(0))
            } else if (MathHelper.distanceFromPlane(triangle(0), planePoint, rot) < 0 && MathHelper.distanceFromPlane(triangle(1), planePoint, rot) > 0){
              insidePoints.append(triangle(1))
//              println("Point 1 : " + triangle(1))
            } else {
              println("cannot be")
            }
          }
          case None            => {
//            println("Point 0 : " + triangle(0))
//            println("Distance from plane: " + MathHelper.distanceFromPlane(triangle(0), planePoint, rot))
            if (MathHelper.distanceFromPlane(triangle(0), planePoint, rot) > 0) {
              insidePoints.append(triangle(0))
            }
            if (MathHelper.distanceFromPlane(triangle(1), planePoint, rot) > 0){
              insidePoints.append(triangle(1))
            }
          }
        }
        MathHelper.intersectPointBetweenPoints(triangle(1), triangle(2), planePoint, rot) match {
          case Some(intersect) => {
            intersectionPoints.append(intersect)
            if (MathHelper.distanceFromPlane(triangle(1), planePoint, rot) > 0 && MathHelper.distanceFromPlane(triangle(2), planePoint, rot) < 0) {
              insidePoints.append(triangle(1))
            } else if (MathHelper.distanceFromPlane(triangle(1), planePoint, rot) < 0 && MathHelper.distanceFromPlane(triangle(2), planePoint, rot) > 0){
              insidePoints.append(triangle(2))
            } else {
              println("cannot be")
            }
          }
          case None            => {
            if (MathHelper.distanceFromPlane(triangle(1), planePoint, rot) > 0) {
              insidePoints.append(triangle(1))
            }
            if (MathHelper.distanceFromPlane(triangle(2), planePoint, rot) > 0){
              insidePoints.append(triangle(2))
            }
          }
        }
        MathHelper.intersectPointBetweenPoints(triangle(2), triangle(0), planePoint, rot) match {
          case Some(intersect) => {
            intersectionPoints.append(intersect)
            if (MathHelper.distanceFromPlane(triangle(2), planePoint, rot) > 0 && MathHelper.distanceFromPlane(triangle(0), planePoint, rot) < 0) {
              insidePoints.append(triangle(2))
            } else if (MathHelper.distanceFromPlane(triangle(2), planePoint, rot) < 0 && MathHelper.distanceFromPlane(triangle(0), planePoint, rot) > 0){
              insidePoints.append(triangle(0))
            } else {
              println("cannot be")
            }
          }
          case None            =>   {
            if (MathHelper.distanceFromPlane(triangle(2), planePoint, rot) > 0) {
              insidePoints.append(triangle(2))
            }
            if (MathHelper.distanceFromPlane(triangle(0), planePoint, rot) > 0){
              insidePoints.append(triangle(0))
            }
          }  
        }
        insidePoints = insidePoints.distinct
//        println(insidePoints)
//        println("Kamera Pos: " + planePoint)
//        println(intersectionPoints)
        
        if (insidePoints.length == 2 && intersectionPoints.length == 2) {
          (Some(Array(insidePoints(0), insidePoints(1), intersectionPoints(0))), Some(Array(insidePoints(1),  intersectionPoints(1), intersectionPoints(0))))
        } else if (insidePoints.length == 1 && intersectionPoints.length == 2) {
          (Some(Array(insidePoints(0), intersectionPoints(0), intersectionPoints(1))), None)
        } else if (insidePoints.length == 3 && intersectionPoints.length == 0) {
          (Some(Array(insidePoints(0), insidePoints(1), insidePoints(2))), None)
        } else if (insidePoints.length == 0) {
          (None, None)
        } else {
          (None, None)
        }
        
        
  }
  
  
  

  
  
  /**
   *  Sorting the vectors ( 3 vectors to represent each end of triangle) by their summed up distance to the kamera.
   */
  def sort(array: Array[Array[VectorVer]]): Array[Array[VectorVer]] = {
                                         array.sortWith((x, y) => {(
                                         sqrt(pow((x(0).validVector(0)(0) - Camera.pos.validVector(0)(0)), 2))
                                       + sqrt(pow((x(0).validVector(1)(0) - Camera.pos.validVector(1)(0)), 2))
                                       + sqrt(pow((x(0).validVector(2)(0) - Camera.pos.validVector(2)(0)), 2))
                                       
                                       + sqrt(pow((x(1).validVector(0)(0) - Camera.pos.validVector(0)(0)), 2))
                                       + sqrt(pow((x(1).validVector(1)(0) - Camera.pos.validVector(1)(0)), 2))
                                       + sqrt(pow((x(1).validVector(2)(0) - Camera.pos.validVector(2)(0)), 2))
                                       
                                       + sqrt(pow((x(2).validVector(0)(0) - Camera.pos.validVector(0)(0)), 2))
                                       + sqrt(pow((x(2).validVector(1)(0) - Camera.pos.validVector(1)(0)), 2))
                                       + sqrt(pow((x(2).validVector(2)(0) - Camera.pos.validVector(2)(0)), 2))
                                       
                                       >
                                         
                                         sqrt(pow((y(0).validVector(0)(0) - Camera.pos.validVector(0)(0)), 2))
                                       + sqrt(pow((y(0).validVector(1)(0) - Camera.pos.validVector(1)(0)), 2))
                                       + sqrt(pow((y(0).validVector(2)(0) - Camera.pos.validVector(2)(0)), 2))
                                       
                                       + sqrt(pow((y(1).validVector(0)(0) - Camera.pos.validVector(0)(0)), 2))
                                       + sqrt(pow((y(1).validVector(1)(0) - Camera.pos.validVector(1)(0)), 2))
                                       + sqrt(pow((y(1).validVector(2)(0) - Camera.pos.validVector(2)(0)), 2))
                                       
                                       + sqrt(pow((y(2).validVector(0)(0) - Camera.pos.validVector(0)(0)), 2))
                                       + sqrt(pow((y(2).validVector(1)(0) - Camera.pos.validVector(1)(0)), 2))
                                       + sqrt(pow((y(2).validVector(2)(0) - Camera.pos.validVector(2)(0)), 2))
    )}
  )}
  
}