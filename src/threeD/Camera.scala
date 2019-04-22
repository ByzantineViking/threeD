package threeD
import scala.collection.mutable.Buffer
import scala.math._

object Camera {
  var rotation = Buffer[Double](0,0,0)
  def horizontal = rotation(1)
  def vertical   = rotation(0)
  
  // Redundant is like rocking a ship.
  def rotate(ver: Double, hor: Double, redundant: Double): Unit = {
    rotation = Buffer(vertical + ver , horizontal + hor , rotation(2) + redundant)
  }
  def roundRotation: Unit = {
    rotation = rotation.map(k => {
      var holder = k/(2*Pi)
      if (holder >= 1.0) {
        k - (floor(holder)*2*Pi)
      } else if (holder <= -1.0) {
        k - (floor(holder)*Pi)
      } else {
        k
      }
    })
  }
  def rotationVector: VectorVer = new VectorVer(Array(Array(sin(horizontal)*1.0), Array(-sin(vertical)* 1.0), Array(cos(horizontal*1.0))))
  
  // The drawing distance. If the last value is negative, everything is inverted horizontally (across a vertical line),
  // and if the last value is positive, everything is inverted vertically (across a horizontal line).
  // The viewing plane in front of camera
  var planePoint: Vector[Double] =  Vector(0,0,-1)
  
  def planePointToVector: VectorVer = new VectorVer(Array(Array(this.x + planePoint(0)),Array(this.z + planePoint(1)),Array(this.y + planePoint(2))))
  
  
  def zoomIn = {
    if (planePoint(2) > -3.0) {
      planePoint = Vector(planePoint(0),planePoint(1),planePoint(2) -0.03)
    }
  }
  def zoomOut = {
    if (planePoint(2) < -0.3) {
      planePoint = Vector(planePoint(0),planePoint(1),planePoint(2) +0.01)
    }
  }
  
  def defaultZoom = {
    planePoint = Vector(0,0,-1)
  }
  
  var pos = new VectorVer(Array(Array(0.0),Array(0.0),Array(0.0)))
  def x: Double = pos.validVector(0)(0)
  def z: Double = pos.validVector(1)(0)
  def y: Double = pos.validVector(2)(0)
  
  def move(xMovement: Double,zMovement: Double,yMovement: Double): Unit = {
    val oldPos = pos
    val possibleNewPos = new VectorVer(Array(Array(pos.x + xMovement),Array(pos.z + zMovement), Array(pos.y + yMovement)))
    for (triangle <- Front.fullData._2) {
        MathHelper.lineInterSectPlane(oldPos, possibleNewPos, triangle(0), MathHelper.normal(triangle).normalize, pow(10, -4)) match {
          case Some(intersection) => {
            val a = MathHelper.distanceFromPlane(intersection, triangle(0), MathHelper.normal(triangle).normalize)
            val b = MathHelper.distanceFromPlane(oldPos, triangle(0), MathHelper.normal(triangle).normalize)
            val c = MathHelper.distanceFromPlane(possibleNewPos, triangle(0), MathHelper.normal(triangle).normalize)
            if (a >= c && a <= b) {
              pos = oldPos
            } else {
              pos = possibleNewPos
            }
          }
          case None               => pos = possibleNewPos
        }
    }
  }
  def moveTo(xPos: Double, zPos: Double, yPos: Double): Unit = {
    pos = new VectorVer(Array(Array(xPos),Array(zPos), Array(yPos)))
  }
}


