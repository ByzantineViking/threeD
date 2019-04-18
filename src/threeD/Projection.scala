package threeD
import scala.collection.mutable.Buffer
import scala.math._


// Converter takes the data, and converts it to right form Array[Array[VectorVer]] for projecting.
object Converter {
  def read: Array[Array[VectorVer]] = {
    var data: Array[Array[Double]] = CSVReader.readCSV
    var vectors = Buffer[Array[VectorVer]]()
    for (line <- data) {
      var shape = line.grouped(3).toArray
      var shapeConverted = Buffer[VectorVer]()
      for (point <- shape) {
        var pointVertical = point.grouped(1).toArray
        shapeConverted += new VectorVer(pointVertical)
      }
      vectors += shapeConverted.toArray
    }
    Sorter.sort(vectors.toArray)
  }
}


// Applies the 2D-planar projection, using 3D projection.
object Projector {
  def project: Array[Array[(Double,Double)]] = {
    val data = Converter.read
    val rotation = Front.kamera.rotation
    val cameraPos = Front.kamera.pos
    def transformationMatrix1 = new Matrix(Array(Array(1,0,0),Array(0, cos(rotation(0)), sin(rotation(0))),Array(0,-sin(rotation(0)),cos(rotation(0)))))
    def transformationMatrix2 = new Matrix(Array(Array(cos(rotation(1)),0,-sin(rotation(1))),Array(0, 1, 0),Array(sin(rotation(1)),0,cos(rotation(1)))))
    def transformationMatrix3 = new Matrix(Array(Array(cos(rotation(2)),sin(rotation(2)),0),Array(-sin(rotation(2)), cos(rotation(2)), 0),Array(0,0,1)))
    val transform = (transformationMatrix1.multiplyWithMatrix(transformationMatrix2)).multiplyWithMatrix(transformationMatrix3)
    val e = Vector(0,0,-0.5)
    var newData = Buffer[Array[VectorVer]]()
    for (shape <- data) {
      var shapes = Buffer[VectorVer]()
      for  (point <- shape) {
        shapes.append(transform.multiplyWithVector(point.minus(cameraPos)))
      }
      newData.append(shapes.toArray)
    }
    val d = newData.toArray
    
    
    val dReformated = Buffer[Array[(Double,Double)]]()
    for (shape <- d ) {
      var shapeReformated = Buffer[(Double,Double)]()
      for ( point <- shape) {
        val x: Double = e(2)/point.y* point.x + e(0)
        val y: Double = e(2)/point.y * point.z + e(1)
        val projected: (Double, Double) = (x,y)
        shapeReformated.append(projected)
      }
      dReformated.append(shapeReformated.toArray)
    }
    dReformated.toArray
  }
  
}