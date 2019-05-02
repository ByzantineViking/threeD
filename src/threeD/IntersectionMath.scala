package threeD

import scala.collection.mutable.Buffer

/**
 * Really heavy intersection math. If you enable this in the projection, the objects behind camera do not show, but on the sides it still bugs out.
 */
object Intersection {
  
  /**
   * Goes through the triangles and gives them for clipping to clip against camera plane.
   */
  def intersectWithCameraPlane(data: Array[Array[VectorVer]], rot:  VectorVer , transform: VectorVer) = {
    var planeCutData =  Buffer[Array[VectorVer]]()
     
      for (triangle <- data) {
  //      // A call to clipping, it has an useful description.
        val point: VectorVer = Camera.pos.plus(transform)
        this.clipping(triangle, point, rot)._1 match {
          case Some(array) => planeCutData.append(array)
          case None        => 
        }
        this.clipping(triangle, point, rot)._2 match {
          case Some(array) => planeCutData.append(array)
          case None        => 
        }
      }
      planeCutData.toArray
  }
  /**
   * Clipping in the 2D phase.
   */
//  def capToScreenSide(triangle: Array[(Double, Double)], sidePaneTop: Vector2D, sidePaneBottom: Vector2D): Buffer[Vector2D] = {
//                
//                val pointA = Vector2D(triangle(0)._1, triangle(0)._2)
//                val pointAB = Vector2D(triangle(1)._1 - triangle(0)._1, triangle(1)._2 - triangle(0)._2)
//                
//                
//                val pointB = Vector2D(triangle(1)._1, triangle(1)._2)
//                val pointBC = Vector2D(triangle(2)._1 - triangle(1)._1, triangle(2)._2 - triangle(1)._2)
//                
//                val pointC = Vector2D(triangle(2)._1, triangle(2)._2)
//                val pointCA = Vector2D(triangle(0)._1 - triangle(2)._1, triangle(0)._2 - triangle(2)._2)
//                
//                
//                
//                val insidePoints = Buffer[Vector2D]()
//                MathHelper.lineInterSectLine(pointA, pointAB, sidePaneTop, sidePaneBottom - sidePaneTop) match {
//                  case (Some(sect1), Some(sect2)) => insidePoints.append(sect1, sect2)
//                  case (Some(sect), None)         => {
//                    insidePoints.append(sect)
//                    val ref = sidePaneBottom.sideOf(sidePaneTop, Vector2D(0,0))
//                    if ((sidePaneBottom.sideOf(sidePaneTop, pointA) >= 0.0 && ref >= 0.0) || (sidePaneBottom.sideOf(sidePaneTop, pointA) <= 0.0 && ref <= 0.0)) {
//                      // Point is inside, ( same side as reference point (0,0), which is middle of the screen)
//                      insidePoints.append(pointA)
//                    } else {
//                      insidePoints.append(pointB)
//                    }
//                  }
//                  case (None, None)               =>
//                  case _ => 
//                }
//                MathHelper.lineInterSectLine(pointB, pointBC, sidePaneTop, sidePaneBottom - sidePaneTop) match {
//                  case (Some(sect1), Some(sect2)) => insidePoints.append(sect1, sect2)
//                  case (Some(sect), None)         => {
//                    insidePoints.append(sect)
//                    val ref = sidePaneBottom.sideOf(sidePaneTop, Vector2D(0,0))
//                    if ((sidePaneBottom.sideOf(sidePaneTop, pointB) >= 0.0 && ref >= 0.0) || (sidePaneBottom.sideOf(sidePaneTop, pointB) <= 0.0 && ref <= 0.0)) {
//                      // Point is inside, ( same side as reference point (0,0), which is middle of the screen)
//                      insidePoints.append(pointB)
//                    } else {
//                      insidePoints.append(pointC)
//                    }
//                  }
//                  case (None, None)               =>
//                  case _ => 
//                }
//                MathHelper.lineInterSectLine(pointC, pointCA, sidePaneTop, sidePaneBottom - sidePaneTop) match {
//                  case (Some(sect1), Some(sect2)) => insidePoints.append(sect1, sect2)
//                  case (Some(sect), None)         => {
//                    insidePoints.append(sect)
//                    val ref = sidePaneBottom.sideOf(sidePaneTop, Vector2D(0,0))
//                    if ((sidePaneBottom.sideOf(sidePaneTop, pointA) >= 0.0 && ref >= 0.0) || (sidePaneBottom.sideOf(sidePaneTop, pointA) <= 0.0 && ref <= 0.0)) {
//                      // Point is inside, ( same side as reference point (0,0), which is middle of the screen)
//                      insidePoints.append(pointB)
//                    } else {
//                      insidePoints.append(pointA)
//                    }
//                  }
//                  case (None, None)               =>
//                  case _ => 
//                }
//                println(insidePoints.distinct)
//                insidePoints.distinct
//              }
//  
  
    /**
   * Takes three points.
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
            intersectionPoints.append(intersect)
            if (MathHelper.distanceFromPlane(triangle(0), planePoint, rot) > 0 && MathHelper.distanceFromPlane(triangle(1), planePoint, rot) < 0) {
              insidePoints.append(triangle(0))
//              println("Point 0 : " + triangle(0))
            } else if (MathHelper.distanceFromPlane(triangle(0), planePoint, rot) < 0 && MathHelper.distanceFromPlane(triangle(1), planePoint, rot) > 0){
              insidePoints.append(triangle(1))
//              println("Point 1 : " + triangle(1))
            } else {
              // Both are already clipped, and set on the plane edge.
              insidePoints.append(triangle(0))
              insidePoints.append(triangle(1))
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
              insidePoints.append(triangle(1))
              insidePoints.append(triangle(2))
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
              insidePoints.append(triangle(2))
              insidePoints.append(triangle(0))
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
        
        
        if (insidePoints.length == 2 && intersectionPoints.length == 2) {
          (Some(Array(insidePoints(0), insidePoints(1), intersectionPoints(0))), Some(Array(insidePoints(1),  intersectionPoints(1), intersectionPoints(0))))
        } else if (insidePoints.length == 1 && intersectionPoints.length == 2) {
          (Some(Array(insidePoints(0), intersectionPoints(1), intersectionPoints(0))), None)
        } else if (insidePoints.length == 3 && intersectionPoints.length == 0) {
          (Some(Array(insidePoints(0), insidePoints(1), insidePoints(2))), None)
        } else if (insidePoints.length == 0) {
          (None, None)
        } else if (insidePoints.length == 3) {
          (Some(triangle) ,None)
        } else {
          (None, None)
        }
        
        
  }
}