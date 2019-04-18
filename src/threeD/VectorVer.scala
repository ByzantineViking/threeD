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
   val validVector = {
     is1x3 match {
       case Some(vector) => vector
       case None          => throw new VectorSizeException({
         var holder = ""
         for (row <- vector) {
           for (i <- row) {
             holder += "i"
           }
           holder += "\n"
         }
         holder
       })
     }
   }
   def x = this.validVector(0)(0)
  def z = this.validVector(1)(0)
  def y = this.validVector(2)(0)
   
   
   def minus(another:VectorVer) = {
     val a = validVector.flatten
     val b = another.validVector.flatten
     new VectorVer(a.zip(b).map { case (x, y) => x - y }.grouped(1).toArray)
   }
   
   def normalize: VectorVer = {
     val length: Double = sqrt(pow(this.x,2) + pow(this.z,2) + pow(this.y,2))
     new VectorVer(Array(Array(this.x /length), Array(this.z/length), Array(this.y/length)))
   }
   
   // Used to calculate normals for visibility calculations.
   def crossProduct(another: VectorVer): VectorVer = {
     new VectorVer(Array(Array(this.z * another.y - this.y * another.z),Array( this.y * another.x - this.x * another.y) , Array(this.x * another.z - this.z * another.x)))
   }
   def dotProduct(another: VectorVer): Double = {
     (this.x * another.x) + (this.z * another.z) + (this.y * another.y)
   }
   
   override def toString()= {
     var output = ""
     for(row <- validVector) {
       output = output ++ (row(0).toString() + "\n")
     }
     output
   }
}
