package threeD
import scala.collection.mutable.Buffer
import scala.math._
object Converter {
  def read = {
    var data: Array[Array[Double]] = CSVReader.readCSV
//    println(data.length)
    var vectors = Buffer[Array[VectorVer]]()
    for (line <- data) {
      var triangle = line.grouped(3).toArray
      var triangleConverted = Buffer[VectorVer]()
      for (point <- triangle) {
        var pointVertical = point.grouped(1).toArray
        triangleConverted += new VectorVer(pointVertical)
      }
      vectors += triangleConverted.toArray
    }
    vectors.toArray
  }
}

object Projector {
  def project: Array[Array[(Double,Double)]] = {
    val data = Converter.read
    val rotation = Front.camera.rotation
    var cameraPos = Front.camera.pos
    def transformationMatrix1 = new Matrix(Array(Array(1,0,0),Array(0, cos(rotation(0)), sin(rotation(0))),Array(0,-sin(rotation(0)),cos(rotation(0)))))
    def transformationMatrix2 = new Matrix(Array(Array(cos(rotation(1)),0,-sin(rotation(1))),Array(0, 1, 0),Array(sin(rotation(1)),0,cos(rotation(1)))))
    def transformationMatrix3 = new Matrix(Array(Array(cos(rotation(2)),sin(rotation(2)),0),Array(-sin(rotation(2)), cos(rotation(2)), 0),Array(0,0,1)))
    val transform = (transformationMatrix1.multiplyWithMatrix(transformationMatrix2)).multiplyWithMatrix(transformationMatrix3)
    
    val e = Vector(0,0,-0.5)
    var newData = Buffer[Array[VectorVer]]()
    for (triangle <- data) {
      var triangles = Buffer[VectorVer]()
      for  (point <- triangle) {
        triangles.append(transform.multiplyWithVector(point.minus(cameraPos)))
      }
      newData.append(triangle.toArray)
    }
    val d = newData.toArray
    val dReformated = Buffer[Array[(Double,Double)]]()
    for (triangle <- d ) {
      var triangleReformated = Buffer[(Double,Double)]()
      for ( point <- triangle) {
        val pointer = point.validVector
        val x: Double = (e(2)/pointer(2)(0))*pointer(0)(0) + e(0)
        val y: Double = (e(2)/pointer(2)(0))*pointer(1)(0) + e(1)
        val projected: (Double, Double) = (x,y)
        triangleReformated.append(projected)
      }
//      triangleReformated.foreach(println(_))
      dReformated.append(triangleReformated.toArray)
    }
    dReformated.toArray
  }
}