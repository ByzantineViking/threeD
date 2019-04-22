package threeD

import scala.collection.mutable.Buffer

/**
 * Really heavy intersection math. If you enable this in the projection, the objects behind camera do not show, but on the sides it still bugs out.
 */
object Intersection {
  
  /**
   * Goes through the triangles and gives them for clipping to clip
   */
  def intersectWithCameraPlane(data: Array[Array[VectorVer]]) = {
    var planeCutData =  Buffer[Array[VectorVer]]()
     
      for (triangle <- data) {
  //      // A call to clipping, it has an useful description.
        val planePointBack: VectorVer = Camera.pos.plus(new VectorVer(Array(Array(0), Array(0), Array(0))))
        val rotStraigth: VectorVer = Camera.rotationVector
        this.clipping(triangle, planePointBack, rotStraigth)._1 match {
          case Some(array) => planeCutData.append(array)
          case None        => 
        }
        this.clipping(triangle, planePointBack, rotStraigth)._2 match {
          case Some(array) => planeCutData.append(array)
          case None        => 
        }
      }
      planeCutData.toArray
  }
  
  
  
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
}