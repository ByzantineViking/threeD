package threeD
import scala.collection.mutable.Buffer
import scala.math._

/**
 * Miscellaneous math regarding 3D spatial calculations, which do not inherently belong to VectorVer.
 */
object MathHelper {
  // Helper function for the projection.
  // Given an Array of triangles in Array[VectorVer] (length == 3) format, produces a normal vector for each of the triangles.
  def normals(array:  Array[Array[VectorVer]]): Array[VectorVer] = {
    var holder: Buffer[VectorVer] = Buffer[VectorVer]()
    for (triangle <- array) {
      // Using cross product to get normal it's important to take first the clock-wise point - base point, then counter-clock-wise point - base point
      holder.append(this.normal(triangle))
    }
    holder.toArray
  } // End of normals
  
  
  // General function for getting a normal for any three points (for example a triangle, which in itself defines a plane)
  def normal(triangle: Array[VectorVer]): VectorVer = {
    (triangle(1).minus(triangle(0))).crossProduct(triangle(2).minus(triangle(0)))
  }
  
  // Preferably accuracy would be somewhere around epsilon so cirka 1*10^-6
  // Takes vector between the two points, and shortens it to the intersection point if one exists, otherwise returns None
  /**
   * The plane normal must be normalized
   * 
   * 
   */
  def lineInterSectPlane(point1: VectorVer, point2: VectorVer, planePoint: VectorVer, planeNormal: VectorVer, accuracy: Double): Option[VectorVer] = {
    val truePlaneNormal = planeNormal.normalize
    // Vector goes from point 1 to point 2    
    val vectorBetweenPoints = point2.minus(point1)
    val dot = truePlaneNormal.dotProduct(vectorBetweenPoints)
    if (abs(dot) > accuracy) {
      // The factor of the point between points
      // If fac is between  (0-1), The points intersects the plane
      //                   < 0.0 : Behind point1
      //                   > 1.0 : In front of point2
      // Because the fac is the plane position relative to the points' order, the points must be given always at the order of the least/(most) distance to the plane.
      val fac: Double = -(truePlaneNormal.dotProduct(point1.minus(planePoint))) / dot
      val hold = vectorBetweenPoints.multiplyWithScalar(fac)
      val res = point1.plus(hold)
      Some(res)
    } else {
      // The line is parallel to the plane.
      None
    }
  } 
  def distanceFromPlane(p: VectorVer, planeP: VectorVer, planeNorm: VectorVer): Double = {
    ((planeNorm.x * p.x) + (planeNorm.z * p.z) + (planeNorm.y * p.y) - (planeP.x * planeNorm.x + planeP.z * planeNorm.z + planeP.y * planeNorm.y))/(sqrt(pow(planeNorm.x,2) + pow(planeNorm.z,2) + pow(planeNorm.y,2)))                  
  }
  /**
   * Takes two points, and returns them as a tuple, closer first ._1
   */
  def whichPointIsCloser(p1: VectorVer, p2:VectorVer, planeP: VectorVer,  planeNorm: VectorVer): (VectorVer, VectorVer) = {
    println(p1.toString() + p2.toString())
    if ( abs(this.distanceFromPlane(p1, planeP, planeNorm)) < abs(this.distanceFromPlane(p2, planeP, planeNorm))) {
      // Point 1 is closer
      (p1,p2)
    } else if (abs(this.distanceFromPlane(p1, planeP, planeNorm)) > abs(this.distanceFromPlane(p2, planeP, planeNorm))){
      // Point 2 is closer
      (p2,p1)
    } else {
      // Points are at equal distance, but very unlikely case that with the accuracy of the double, they are at equal dist.
      // This is handled at the next point of calculation, in lineInterSectPlane, within a certain accuracy.
      // So at this point, the points are returned in the original order.
      (p1,p2)
    }
  }
  
  
}