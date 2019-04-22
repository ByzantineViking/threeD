package threeD

import scala.math._

class VectorVer(val vector: Array[Array[Double]]) {
    def is1x3 : Option[Array[Array[Double]]] = {
      if (vector.length == 3 && vector.map(_.length).forall(_ == 1)) {
        Some(vector)
      } else {
        None
      }
    }
   val validVector: Array[Array[Double]] = {
     this.is1x3 match {
       case Some(vector) => vector
       case None          => throw new VectorSizeException(this.toString())
     }
   }
   def x = this.validVector(0)(0)
   def z = this.validVector(1)(0)
   def y = this.validVector(2)(0)
   
   def plus(another:VectorVer) = {
     val a = validVector.flatten
     val b = another.validVector.flatten
     new VectorVer(a.zip(b).map { case (x, y) => x + y }.grouped(1).toArray)
   }
   
   def minus(another:VectorVer) = {
     val a = validVector.flatten
     val b = another.validVector.flatten
     new VectorVer(a.zip(b).map { case (x, y) => x - y }.grouped(1).toArray)
   }
   def multiplyWithScalar(scalar: Double): VectorVer = {
     new VectorVer(Array(Array(this.x * scalar), Array(this.z * scalar), Array(this.y * scalar)))
   }
   def normalize: VectorVer = {
     new VectorVer(Array(Array(this.x /this.length), Array(this.z/this.length), Array(this.y/this.length)))
   }
   
   def switchSign: VectorVer = {
     new VectorVer(Array(Array(-this.x), Array(-this.z), Array(-this.y)))
   }
   
   def abs: VectorVer = {
     new VectorVer(Array(Array(this.x.abs), Array(this.z.abs), Array(this.y.abs)))
   }
   
   def equal(another: VectorVer) = {
     if (this.x == another.x && this.z == another.z && this.y == another.y) {
       true
     } else {
       false
     }
   }
   
   // Used to calculate normals for visibility calculations.
   def crossProduct(another: VectorVer): VectorVer = {
     new VectorVer(Array(Array(this.z * another.y - this.y * another.z),Array( this.y * another.x - this.x * another.y) , Array(this.x * another.z - this.z * another.x)))
   }
   def dotProduct(another: VectorVer): Double = {
     (this.x * another.x) + (this.z * another.z) + (this.y * another.y)
   }
   
   def length: Double = {
     sqrt(pow(this.x,2) + pow(this.z,2) + pow(this.y,2))
   }
   override def toString()= {
     "\n" + this.x.toString() + " , " + this.z.toString() + " , " + this.y.toString() + "\n"
   }
}
// Factory methods
object VectorVer {
  def createZeroVector: VectorVer = {
    new VectorVer(Array(Array(0.0), Array(0.0), Array(0.0)))
  }
  def apply(x: Double, z: Double, y: Double) = {
    new VectorVer(Array(Array(x), Array(z), Array(y)))
  }
}
