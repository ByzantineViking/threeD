package threeD

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
       case None          => throw new VectorSizeException(vector.toString())
     }
   }
   def minus(another:VectorVer) = {
     val a = validVector.flatten
     val b = another.validVector.flatten
     new VectorVer(a.zip(b).map { case (x, y) => x - y }.grouped(1).toArray)
   }
   
   override def toString()= {
     var output = ""
     for(row <- validVector) {
       output = output ++ (row(0).toString() + "\n")
     }
     output
   }
}