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
  def clipping(triangle : Array[VectorVer], kamera: Camera): (Option[Array[VectorVer]],Option[Array[VectorVer]]) = {
        var insidePoints = Buffer[VectorVer]()
        var outsidePoints = Buffer[VectorVer]()
        var intersectionPoints = Buffer[VectorVer]()
         
        val planePoint: VectorVer = kamera.pos.plus(new VectorVer(Array(Array(0), Array(0), Array(0.0))))
        val rot: VectorVer = kamera.rotationVector
        helpClip(triangle(0), triangle(1), planePoint, rot) match {
          case Some(intersect) => {
            intersectionPoints.append(intersect)
            if (MathHelper.distanceFromPlane(triangle(0), planePoint, rot) > 0 && MathHelper.distanceFromPlane(triangle(1), planePoint, rot) < 0) {
              insidePoints.append(triangle(0))
            } else if (MathHelper.distanceFromPlane(triangle(0), planePoint, rot) < 0 && MathHelper.distanceFromPlane(triangle(1), planePoint, rot) > 0){
              insidePoints.append(triangle(1))
            } else {
              println("cannot be")
            }
          }
          case None            => {
            if (MathHelper.distanceFromPlane(triangle(0), planePoint, rot) > 0) {
              insidePoints.append(triangle(0))
            }
            if (MathHelper.distanceFromPlane(triangle(1), planePoint, rot) > 0){
              insidePoints.append(triangle(1))
            }
          }
        }
        helpClip(triangle(1), triangle(2), planePoint, rot) match {
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
        helpClip(triangle(2), triangle(0), planePoint, rot) match {
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
        
        println("Inside : " + insidePoints.length)
        println("Intersect : " + intersectionPoints.length)
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
  
  def helpClip(p0: VectorVer, p1: VectorVer, planePoint: VectorVer, rot : VectorVer): Option[VectorVer] = {
    MathHelper.lineInterSectPlane(p0, p1, planePoint, rot , pow(10, -6)) match {
      case Some(intersectPoint) => {
        val cmp: Double = p1.minus(p0).dotProduct(intersectPoint.minus(p0))
        if (cmp > 0.0 && cmp < sqrt(p1.minus(p0).length)) {
          Some(intersectPoint)
        } else {
          None
        }
      }
      case None            => None
      
    }
  }
  

  
  
  /**
   *  Sorting the vectors ( 3 vectors to represent each end of triangle) by their summed up distance to the kamera.
   */
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