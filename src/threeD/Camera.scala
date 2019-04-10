package threeD
import scala.collection.mutable.Buffer
class Camera {
  var rotation = Buffer[Double](0.0,0.0,0.0)
  def rotate(xRotation: Double, yRotation: Double, zRotation: Double) = {
    rotation = Buffer(rotation(0) + xRotation , rotation(1) + yRotation , rotation(2) + zRotation)
  }
  
  var pos = new VectorVer(Array(Array(0.0),Array(0.0),Array(0.0)))
  def move(xMovement: Double,yMovement: Double,zMovement: Double) = {
    var temp = pos.validVector
    pos = new VectorVer(Array(Array(temp(0)(0) + xMovement),Array(temp(1)(0) + yMovement), Array(temp(2)(0) + zMovement)))
  }
  def resetPos = {
    pos = new VectorVer(Array(Array(0), Array(0), Array(0)))
  }
}