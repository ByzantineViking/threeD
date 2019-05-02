package threeD
import scala.collection.mutable.Buffer
import scala.math._
/**
 * The "player" object. Can move and look around in the world.
 */
object Camera {
  var rotation = Buffer[Double](0,0,0)
  def horizontal = rotation(1)
  def vertical   = rotation(0)
  def leaning    = rotation(2)
  // Redundant is like rocking a ship.
  def rotate(ver: Double, hor: Double, rocking: Double): Unit = {
    var trueVer = ver
    if (vertical > 0.8 && ver > 0.0) {
      trueVer = 0.0
    } else if (vertical < -0.7 && ver < -0.0) {
      trueVer = 0.0
    }
    rotation = Buffer(vertical + trueVer , horizontal + hor , leaning + rocking)
  }
  
  def resetLeaning: Unit = {
    rotation(2) = 0.0
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
  
  /**
   * Helper to get the side and top/bottom 3D clippings against planes.
   */
  def rotatedRotationVector(ver: Double, hor: Double) : VectorVer = VectorVer(sin(horizontal + hor), -sin(vertical + ver), cos(horizontal + hor))
  
  
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
    var moving = true
    for (triangle <- Front.fullData._2) {
      MathHelper.lineInterSectPlane(oldPos, possibleNewPos, triangle(0), MathHelper.normal(triangle), pow(10,-6)) match {
        case Some(sect) => {
          if (MathHelper.minsAndMaxesOfTriangleCoordinates(sect, triangle)) {
            if (MathHelper.normal(triangle).normalize.dotProduct(possibleNewPos.minus(oldPos).normalize) < 0) {
              if (MathHelper.distanceFromPlane(possibleNewPos, triangle(0), MathHelper.normal(triangle).normalize) < 0.1) {
                moving = false
              }
            }
            
          }  else {
          }
        }
        case None       => {
          
        }
      }
    }
    if (moving) {
      pos = possibleNewPos
    }
  }
  def moveTo(xPos: Double, zPos: Double, yPos: Double): Unit = {
    pos = new VectorVer(Array(Array(xPos),Array(zPos), Array(yPos)))
  }
  
}


