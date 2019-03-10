package threeD
import scala.collection.mutable.Buffer
class Matrix(val matrix : Array[Array[Double]] ) {
  def is3x3 : Option[Array[Array[Double]]] = {
    if (matrix.length == 3 && matrix.map(_.length).forall(_ == 3)) {
      Some(matrix)
    } else {
      None
    }
  }
   val validMatrix = {
     is3x3 match {
       case Some(matrix) => matrix
       case None          => throw new MatrixSizeException(matrix.toString())
     }
   }
    
   def multiplyWithMatrix(another: Matrix) = {
     val result = Buffer[Array[Double]]()
     var columnNumber = 0
     for (row <- validMatrix) {
       columnNumber = 0
       val midResult = Buffer[Double]()
       var value: Double = 0
       value += row(0) * another.validMatrix(0)(columnNumber)
       value += row(1) * another.validMatrix(1)(columnNumber)
       value += row(2) * another.validMatrix(2)(columnNumber)
       midResult += value
       columnNumber += 1
       value = 0
       value += row(0) * another.validMatrix(0)(columnNumber)
       value += row(1) * another.validMatrix(1)(columnNumber)
       value += row(2) * another.validMatrix(2)(columnNumber)
       midResult += value
       columnNumber += 1
       value = 0
       value += row(0) * another.validMatrix(0)(columnNumber)
       value += row(1) * another.validMatrix(1)(columnNumber)
       value += row(2) * another.validMatrix(2)(columnNumber)
       midResult += value
       result += midResult.toArray
     }
     new Matrix(result.toArray)
   }
   
   def multiplyWithVector(another: VectorVer): VectorVer = {
     val result = Buffer[Array[Double]]()
     for (row <- validMatrix) {
       var value: Double = 0
       value += row(0) * another.validVector(0)(0)
       value += row(1) * another.validVector(1)(0)
       value += row(2) * another.validVector(2)(0)
       result += Array(value)
     }
     new VectorVer(result.toArray)
   }
   def transpoosi = new Matrix(validMatrix.transpose)
   override def toString()= {
     var output = ""
     for(row <- validMatrix) {
       var rowString = ""
       for(element <- row) {
         rowString = rowString ++ element.toString() ++ ","
       }
       output += rowString + "\n"
     }
     output
   }
}

