package threeD

import scala.math._
import scala.collection.mutable.Buffer
object Sorter {
  
  
  def clipping(triangle : Array[VectorVer], kamera: Camera) = {
    var insidePoints = Buffer[VectorVer]()
        var outsidePoints = Buffer[VectorVer]()
        var intersectionPoints = Buffer[VectorVer]()
        // Check all the three sides of the triangle
        MathHelper.lineInterSectPlane(triangle(0), triangle(1), kamera.planePointToVector, kamera.rotationVector, pow(10, -6)) match {
          // Lines intersect
          case Some((fac, res)) => {
            // On the continuation of the line, front of plane
            if (fac > 1.0) {
              insidePoints += triangle(0)
              insidePoints += triangle(1)
            // On the continuation of the line, back of plane
            } else if (fac < 0.0) {
              outsidePoints += triangle(0)
              outsidePoints += triangle(1)
            // On the line, at the point indicated by res
            } else {
              insidePoints += triangle(0)
              outsidePoints += triangle(1)
              intersectionPoints  += res
            }
          }
          // To the accuracy of the lineintersectPlane last constructor, the line and plane are parallel.
          case None               => {
            val whichSide =  MathHelper.distanceFromPlane(triangle(0), kamera.planePointToVector, kamera.rotationVector)
            // Line lies front of plane
            if (whichSide < 0) {
              insidePoints += triangle(0)
              insidePoints += triangle(1)
            // Line lies back of plane
            } else {
              outsidePoints += triangle(0)
              outsidePoints += triangle(1)
            }
          }
        }
        // Second side of triangle
        MathHelper.lineInterSectPlane(triangle(1), triangle(2), kamera.planePointToVector, kamera.rotationVector, pow(10, -6)) match {
          case Some((fac, res)) => {
            if (fac > 1.0) {
              insidePoints += triangle(1)
              insidePoints += triangle(2)
            } else if (fac < 0.0) {
              outsidePoints += triangle(1)
              outsidePoints += triangle(2)
            } else {
              insidePoints += triangle(1)
              outsidePoints += triangle(2)
              intersectionPoints  += res
            }
          }
          case None               => {
            val whichSide =  MathHelper.distanceFromPlane(triangle(1), kamera.planePointToVector, kamera.rotationVector)
            if (whichSide < 0) {
              insidePoints += triangle(1)
              insidePoints += triangle(2)
            } else {
              outsidePoints += triangle(1)
              outsidePoints += triangle(2)
            }
          }
        }
        // Third side of triangle
        MathHelper.lineInterSectPlane(triangle(2), triangle(0), kamera.planePointToVector, kamera.rotationVector, pow(10, -6)) match {
          case Some((fac, res)) => {
            if (fac > 1.0) {
              insidePoints += triangle(2)
              insidePoints += triangle(0)
            } else if (fac < 0.0) {
              outsidePoints += triangle(2)
              outsidePoints += triangle(0)
            } else {
              insidePoints += triangle(2)
              outsidePoints += triangle(0)
              intersectionPoints  += res
            }
          }
          case None               => {
            val whichSide =  MathHelper.distanceFromPlane(triangle(2), kamera.planePointToVector, kamera.rotationVector)
            if (whichSide < 0) {
              insidePoints += triangle(2)
              insidePoints += triangle(0)
            } else {
              outsidePoints += triangle(2)
              outsidePoints += triangle(0)
            }
          }
        }
        
        val inside: Int = insidePoints.distinct.length
        val outside: Int= outsidePoints.distinct.length
        println("-----------")
        println(inside)
        println(outside)
        println(intersectionPoints.distinct)
        println("-----------")
        if (inside == 2) {
          planeCutData.append(insidePoints.distinct(0), insidePoints.distinct(1), intersectionPoints.distinct(0))
          planeCutData.append(insidePoints.distinct(1), insidePoints.distinct(1), intersectionPoints.distinct(0))
        } else if (inside == 1) {
          planeCutData.append(insidePoints.distinct(0), intersectionPoints.distinct(0), intersectionPoints.distinct(1))
        } else if (inside == 0) {
          // Triangle not drawn
        } else if (inside == 3) {
          // Triangle drawn
          planeCutData.append(insidePoints.distinct(0), insidePoints.distinct(1), insidePoints.distinct(2))
        } 
  }
  
  
  
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