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
   * The factorial part doesn't work.. Might be differences in how the 3d space is constructed. The intersection point still remains accurate.
   * I've tried another way and it did not produce an accurate point, so this will do in the limitations of the axioms of the 3d world.
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
  /**
   * Questionable working condition
   */
  def intersectPointBetweenPoints(p0: VectorVer, p1: VectorVer, planePoint: VectorVer, rot : VectorVer): Option[VectorVer] = {
    MathHelper.lineInterSectPlane(p0, p1, planePoint, rot , pow(10, -2)) match {
      case Some(intersectPoint) => {
        val crossProd = (p1.minus(p0)).crossProduct(intersectPoint.minus(p0))
        val cmp: Double = p1.minus(p0).dotProduct(intersectPoint.minus(p0))
        if (crossProd.abs.equal(VectorVer.createZeroVector) && cmp > 0.0 && cmp < sqrt(p1.minus(p0).length)) {
          Some(intersectPoint)
          
        } else {
          None
        }
      }
      case None            => None
      
    }
  }
  def minsAndMaxesOfTriangleCoordinates(sect: VectorVer, triangle: Array[VectorVer]): Boolean = {
    val minX = min(min(triangle(0).x, triangle(1).x), triangle(2).x)
    val minY = min(min(triangle(0).y, triangle(1).y), triangle(2).y)
    val minZ = min(min(triangle(0).z, triangle(1).z), triangle(2).z)
    
    val maxX = max(max(triangle(0).x, triangle(1).x), triangle(2).x)
    val maxY = max(max(triangle(0).y, triangle(1).y), triangle(2).y)
    val maxZ = max(max(triangle(0).z, triangle(1).z), triangle(2).z)
    
    
    
    if (sect.x >= minX && sect.y >= minY && sect.z >= minZ) {
      if (sect.x <= maxX && sect.y <= maxY && sect.z <= maxZ) {
        true
      } else {
        false
      }
    } else {
      false
    }
  }
 
  def pointDistanceFromCameraPoint(vec: VectorVer): Double =  {
    val cam = Camera.pos
    sqrt(pow((vec.x - cam.x), 2) + pow((vec.z - cam.z), 2) + pow((vec.y - cam.y), 2)) 
  }
  def triangleDistanceFromCameraPoint(triangle: Array[VectorVer]): Double =  {
    val cam = Camera.pos
    sqrt(pow((triangle(0).x - cam.x), 2)) + sqrt(pow((triangle(0).z - cam.z), 2)) + sqrt(pow((triangle(0).y - cam.y), 2))
  }
  
  
  def centerPointOfTriangle(triangle: Array[VectorVer]) : VectorVer = {
    VectorVer((triangle(0).x + triangle(1).x + triangle(2).x)/3.0  , (triangle(0).z + triangle(1).z + triangle(2).z)/3.0  ,  (triangle(0).y + triangle(1).y + triangle(2).y)/3.0)
  }
  
  
  
  
  
  
  
//------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------//
  // Under here is a 2D plane clip-helper, and a 3D intersection algorithm with an edge case problem.
    
  /**
   * https://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
   */
  
//  def lineInterSectLine(q: Vector2D, s: Vector2D, p: Vector2D, r: Vector2D): (Option[Vector2D],Option[Vector2D])  = {
//    val t = (q - p).cross(s / (r.cross(s)))
//    val u = (q - p).cross(r / (r.cross(s)))
//    
//    if (r.cross(s) == 0.0 && ((q-p).cross(r) == 0.0)) {
//      // Co-linear
//      if (s*r < 0.0) {
//        // S and R point in different directions
//        val t1 = (q - p) * ( r / (r * r))
//        val t0 = t1 + ((s * r) / (r * r))
//        if (t0 >= 0 && t0 <= 1 && t1 >= 0 && t1 <= 1 ) {
//          (Some(q), Some(q + s))
//        } else {
//          // Colinear and disjoint, no intersection
//          (None, None)
//        }
//      } else {
//        // In the same direction
//        val t0 = (q - p) * ( r / (r * r))
//        val t1 = t0 + ((s * r) / (r * r))
//        if (t0 >= 0 && t0 <= 1 && t1 >= 0 && t1 <= 1 ) {
//          (Some(q), Some(q + s))
//        } else {
//          // Colinear and disjoint, no intersection
//          (None, None)
//        }
//      }
//      
//    } else if (r.cross(s) == 0.0 && (q-p).cross(r) != 0) {
//      (None, None)
//      // Parallel and not-intersecting
//    } else if( r.cross(s) != 0.0 && 0.0 <= t && t <= 1.0 && 0.0 <= u && u <= 1.0 ) {
//      // Intersect
//      // Should be the same as q + s.scalar(u)
//      (Some(p + r.scalar(t)), None)
//    } else {
//      // Not parallel, no intersection
//      (None, None)
//    }
//  }

  /**
   * For defining collision.
   */
//  def lineSegmentIntersectWithATriangle(p0: VectorVer, p1: VectorVer, triangle: Array[VectorVer]): Boolean = {
//   val t0 = triangle(0)
//   val t1  = triangle(1)
//   val t2 = triangle(2)
//   
//   val dXY = t0.x* (t1.z - t2.z) + t1.x*(t2.z -t0.z) - t2.x*(t1.z-t0.z)
//   val dXZ = t0.x* (t1.y - t2.y) + t1.x*(t2.y -t0.y) - t2.x*(t1.y-t0.y)
//   val dYZ = t0.z* (t1.y - t2.y) + t1.z*(t2.y -t0.y) - t2.z*(t1.y-t0.y)
//   
//   // XY biggest
//   if (dXY.abs >= dYZ.abs && dXY.abs >= dXZ.abs) {
//     val Uu = (t2.x*t0.z - t0.x*t2.z)/ dXY
//     val Ux = (t2.z - t0.z) / dXY
//     val Uy = (t0.x- t2.x)/dXY
//     val Uz = 0
//     
//     val Vu = (t0.x * t1.z - t1.x * t0.z) / dXY
//     val Vx = (t0.z -t1.z) / dXY
//     val Vy = (t1.x - t0.x) / dXY
//     val Vz = 0
//     
//     
//     def helper(x: Double, y: Double, z: Double) = {
//       val u = Uu + x*Ux + y*Uy + z* Uz
//       val v = Vu + x*Vx + y*Vy + z*Vz
//       (u, v)
//     }
//     // p0 starting point, p1 ending point
//     val u0 = helper(p0.x,p0.z,p0.y)._1
//     val v0 = helper(p0.x,p0.z,p0.y)._2
//     val u1 = helper(p1.x,p1.z,p1.y)._1
//     val v1 = helper(p1.x,p1.z,p1.y)._2
//     
//     if ((u0 == 0) && (u1 == 0)) {
//       true
//     } else if ((v0 == 0) && (v1 == 0)) {
//       true
//     } else if (u0 + v0 == 1 && u1 + v1 == 1) {
//       true
//     } else if ((u0 <= 0 && u1 > 0) || (u0 >= 0 && u1 < 0)) {
//       val tu = u0/(u0-u1)
//       if ((tu >= 0 && tu <= 1) && ((1-tu)*v0 + tu *v1 >= 0 && (1-tu)*v0 + tu *v1 <= 1)) {
//         true
//       } else {
//         false
//       }
//     } else if ((v0 <= 0 && v1 > 0) || (v0 >= 0 && v1 < 0)) {
//       val tv = v0/(v0-v1)
//       if ((tv >= 0 && tv <= 1) && ((1-tv)*v0 + tv *v1 >= 0 && (1-tv)*v0 + tv *v1 <= 1)) {
//         true
//       } else {
//         false
//       }
//     } else if (((1-u0-v0 >= u1 - u0 + v1 -v0) && ( u1 - u0 + v1 -v0 > 0)) || ((u0 + v0 - 1 >= u0 - u1 + v0 - v1) && (u0 - u1 + v0 - v1 > 0))) {
//       val tuv = (1-u0-v0)/ (u1-u0+v1-v0)
//       if ( tuv >= 0 && tuv <= 1) {
//         if ((0 <= (1-tuv) * u0 + tuv * u1) && ((1-tuv) * u0 + tuv * u1 <= 1)) {
//           if ((0 <= (1-tuv) * v0 + tuv * v1) && ((1-tuv) * v0 + tuv * v1 <= 1) ) {
//             true
//           } else {
//             false
//           }
//         } else {
//           false
//         }
//       } else {
//         false
//       }
//     } else {
//       false
//     }
//     
//   // XZ biggest
//   } else if (dXZ.abs >= dXY.abs && dXZ.abs >= dYZ.abs) {
//     val Uu = (t2.x*t0.y - t0.x*t2.y)/ dXZ
//     val Ux = (t2.y - t0.y) / dXZ
//     val Uy = 0 
//     val Uz = (t0.x- t2.x)/dXZ
//     
//     val Vu = (t0.x * t1.y - t1.x * t0.y) / dXZ
//     val Vx = (t0.y -t1.y) / dXZ
//     val Vy = 0
//     val Vz = (t1.x - t0.x) / dXZ
//     
//     def helper(x: Double, y: Double, z: Double) = {
//       val u = Uu + x*Ux + y*Uy + z* Uz
//       val v = Vu + x*Vx + y*Vy + z*Vz
//       (u, v)
//     }
//     // p0 starting point, p1 ending point
//     val u0 = helper(p0.x,p0.z,p0.y)._1
//     val v0 = helper(p0.x,p0.z,p0.y)._2
//     val u1 = helper(p1.x,p1.z,p1.y)._1
//     val v1 = helper(p1.x,p1.z,p1.y)._2
//     
//     if ((u0 == 0) && (u1 == 0)) {
//       true
//     } else if ((v0 == 0) && (v1 == 0)) {
//       true
//     } else if (u0 + v0 == 1 && u1 + v1 == 1) {
//       true
//     } else if ((u0 <= 0 && u1 > 0) || (u0 >= 0 && u1 < 0)) {
//       val tu = u0/(u0-u1)
//       if ((tu >= 0 && tu <= 1) && ((1-tu)*v0 + tu *v1 >= 0 && (1-tu)*v0 + tu *v1 <= 1)) {
//         true
//       } else {
//         false
//       }
//     } else if ((v0 <= 0 && v1 > 0) || (v0 >= 0 && v1 < 0)) {
//       val tv = v0/(v0-v1)
//       if ((tv >= 0 && tv <= 1) && ((1-tv)*v0 + tv *v1 >= 0 && (1-tv)*v0 + tv *v1 <= 1)) {
//         true
//       } else {
//         false
//       }
//     } else if (((1-u0-v0 >= u1 - u0 + v1 -v0) && ( u1 - u0 + v1 -v0 > 0)) || ((u0 + v0 - 1 >= u0 - u1 + v0 - v1) && (u0 - u1 + v0 - v1 > 0))) {
//       val tuv = (1-u0-v0)/ (u1-u0+v1-v0)
//       if ( tuv >= 0 && tuv <= 1) {
//         if ((0 <= (1-tuv) * u0 + tuv * u1) && ((1-tuv) * u0 + tuv * u1 <= 1)) {
//           if ((0 <= (1-tuv) * v0 + tuv * v1) && ((1-tuv) * v0 + tuv * v1 <= 1) ) {
//             true
//           } else {
//             false
//           }
//         } else {
//           false
//         }
//       } else {
//         false
//       }
//     } else {
//       false
//     }
//     
//     
//   // YZ biggest
//   } else {
//     val Uu = (t2.z*t0.y - t0.z*t2.y)/ dYZ
//     val Ux = 0
//     val Uy = (t2.y - t0.y) / dYZ
//     val Uz = (t0.z- t2.z)/dYZ
//     
//     val Vu = (t0.z * t1.y - t1.z * t0.y) / dYZ
//     val Vx = 0
//     val Vy = (t0.y -t1.y) / dYZ
//     val Vz = (t1.z - t0.z) / dYZ
//     
//     def helper(x: Double, y: Double, z: Double) = {
//       val u = Uu + x*Ux + y*Uy + z* Uz
//       val v = Vu + x*Vx + y*Vy + z*Vz
//       (u, v)
//     }
//     // p0 starting point, p1 ending point
//     val u0 = helper(p0.x,p0.z,p0.y)._1
//     val v0 = helper(p0.x,p0.z,p0.y)._2
//     val u1 = helper(p1.x,p1.z,p1.y)._1
//     val v1 = helper(p1.x,p1.z,p1.y)._2
//     
//     if ((u0 == 0) && (u1 == 0)) {
//       true
//     } else if ((v0 == 0) && (v1 == 0)) {
//       true
//     } else if (u0 + v0 == 1 && u1 + v1 == 1) {
//       true
//     } else if ((u0 <= 0 && u1 > 0) || (u0 >= 0 && u1 < 0)) {
//       val tu = u0/(u0-u1)
//       if ((tu >= 0 && tu <= 1) && ((1-tu)*v0 + tu *v1 >= 0 && (1-tu)*v0 + tu *v1 <= 1)) {
//         true
//       } else {
//         false
//       }
//     } else if ((v0 <= 0 && v1 > 0) || (v0 >= 0 && v1 < 0)) {
//       val tv = v0/(v0-v1)
//       if ((tv >= 0 && tv <= 1) && ((1-tv)*v0 + tv *v1 >= 0 && (1-tv)*v0 + tv *v1 <= 1)) {
//         true
//       } else {
//         false
//       }
//     } else if (((1-u0-v0 >= u1 - u0 + v1 -v0) && ( u1 - u0 + v1 -v0 > 0)) || ((u0 + v0 - 1 >= u0 - u1 + v0 - v1) && (u0 - u1 + v0 - v1 > 0))) {
//       val tuv = (1-u0-v0)/ (u1-u0+v1-v0)
//       if ( tuv >= 0 && tuv <= 1) {
//         if ((0 <= (1-tuv) * u0 + tuv * u1) && ((1-tuv) * u0 + tuv * u1 <= 1)) {
//           if ((0 <= (1-tuv) * v0 + tuv * v1) && ((1-tuv) * v0 + tuv * v1 <= 1) ) {
//             true
//           } else {
//             false
//           }
//         } else {
//           false
//         }
//       } else {
//         false
//       }
//     } else {
//       false
//     }
//   }
//  }
  
}