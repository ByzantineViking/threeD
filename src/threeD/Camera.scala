package threeD
import scala.collection.mutable.Buffer
class Camera {
  var rotation = Buffer[Double](0,0,0)
  def rotate(xRotation: Double, zRotation: Double, yRotation: Double) = {
    rotation = Buffer(rotation(0) + xRotation , rotation(1) + zRotation , rotation(2) + yRotation)
  }
  var pos = new VectorVer(Array(Array(0.0),Array(0.0),Array(0.0)))
  def x = pos.validVector(0)(0)
  def z = pos.validVector(1)(0)
  def y = pos.validVector(2)(0)
  
  def move(xMovement: Double,zMovement: Double,yMovement: Double) = {
    var temp = pos.validVector
    pos = new VectorVer(Array(Array(temp(0)(0) + xMovement),Array(temp(1)(0) + zMovement), Array(temp(2)(0) + yMovement)))
  }
  def resetPos = {
    pos = new VectorVer(Array(Array(0), Array(0), Array(0)))
  }
}