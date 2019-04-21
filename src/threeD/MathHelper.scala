package threeD
import scala.collection.mutable.Buffer
import scala.math._
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
  def lineInterSectPlane(point1: VectorVer, point2: VectorVer, planePoint: VectorVer, planeNormal: VectorVer, accuracy: Double): Option[(Double, VectorVer)] = {
    val truePlaneNormal = planeNormal.normalize
    // Vector goes from point 1 to point 2    
    val vectorBetweenPoints = point2.minus(point1)
    val dot = truePlaneNormal.dotProduct(vectorBetweenPoints)
    if (abs(dot) > accuracy) {
      // The factor of the point between points
      // If fac is between  (0-1), the points intersects the plane
      //                   < 0.0 : behind the plane
      //                   > 1.0 : In front of plane
      val fac: Double = -(truePlaneNormal.dotProduct(point1.minus(planePoint))) / dot
      val hold = vectorBetweenPoints.multiplyWithScalar(fac)
      val res = point1.plus(hold)
      Some(fac, res)
    } else {
      // The line is parallel to the plane.
      None
    }
  } 
  
  def distanceFromPlane(p: VectorVer, planeP: VectorVer, planeNorm: VectorVer): Double = {
    ((planeNorm.x * p.x) + (planeNorm.z * p.z) + (planeNorm.y * p.y) - (planeP.x * planeNorm.x + planeP.z * planeNorm.z + planeP.y * planeNorm.y))/(sqrt(pow(planeNorm.x,2) + pow(planeNorm.z,2) + pow(planeNorm.y,2)))                  
  }
  
  
}