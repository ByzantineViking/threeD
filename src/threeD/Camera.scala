package threeD
import scala.collection.mutable.Buffer
class Camera {
  var rotation = Buffer[Double](0,0,0)
  def rotate(xRotation: Double, zRotation: Double, yRotation: Double) = {
    rotation = Buffer(rotation(0) + xRotation , rotation(1) + zRotation , rotation(2) + yRotation)
  }
  var pos = new VectorVer(Array(Array(0.0),Array(0.0),Array(0.0)))
  def x: Double = pos.validVector(0)(0)
  def z: Double = pos.validVector(1)(0)
  def y: Double = pos.validVector(2)(0)
  
  def move(xMovement: Double,zMovement: Double,yMovement: Double) = {
    pos = new VectorVer(Array(Array(pos.x + xMovement),Array(pos.z + zMovement), Array(pos.y + yMovement)))
  }
  def moveTo(xPos: Double, zPos: Double, yPos: Double) = {
    pos = new VectorVer(Array(Array(xPos),Array(zPos), Array(yPos)))
  }
}