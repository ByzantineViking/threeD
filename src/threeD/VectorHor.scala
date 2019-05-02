package threeD
/**
 * Helper class for the clipping against the edges of the canvas.
 */
class Vector2D(val x: Double,val y: Double) {
  def cross(another: Vector2D) = {
    this.x * another.y - this.y * another.x
  }
  def /(d : Double) = {
    Vector2D(this.x/d, this.y/d)
  }
  def +(another: Vector2D) = {
    Vector2D(this.x + another.x, this.y + another.y)
  }
  def -(another: Vector2D) = {
    Vector2D(this.x + another.x, this.y + another.y)
  }
  def *(another: Vector2D) = {
    this.x * another.x + this.y * another.y
  }
  def scalar(s: Double) = {
    Vector2D(x*s, y*s)
  }
  
  /**
   * This Vector2D is a startingPoint.
   * Two points on different sides get a different sign from this method.
   * To gain useful information, take a reference point, whose sign you know for sure.
   */
  def sideOf(endPoint: Vector2D, point: Vector2D): Double = {
    val d = (point.x - this.x)*(endPoint.y - this.y) - (point.y - this.y)*(endPoint.x - this.x)
    d
  }
  
}



object Vector2D {
  def apply(x: Double, y: Double) = {
    new Vector2D(x,y)
  }
  
}