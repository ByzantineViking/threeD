package threeD
import scala.collection.mutable.Buffer
import scala.math._
object Converter {
  def read = {
    var data: Array[Array[Double]] = CSVReader.readCSV
//    println(data.length)
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

object Sorter {
  
  // Sorting the vectors ( 3 vectors to represent each end of triangle) by their distance to the kamera
  def sort(array: Array[Array[VectorVer]]): Array[Array[VectorVer]] = {
                                         array.sortWith((x, y) => {(
                                         sqrt(pow((x(0).validVector(0)(0) - Front.kamera.pos.validVector(0)(0)), 2))
                                       + sqrt(pow((x(0).validVector(1)(0) - Front.kamera.pos.validVector(1)(0)), 2))
                                       + sqrt(pow((x(0).validVector(2)(0) - Front.kamera.pos.validVector(2)(0)), 2))
                                       
                                       + sqrt(pow((x(1).validVector(0)(0) - Front.kamera.pos.validVector(0)(0)), 2))
                                       + sqrt(pow((x(1).validVector(1)(0) - Front.kamera.pos.validVector(1)(0)), 2))
                                       + sqrt(pow((x(1).validVector(2)(0) - Front.kamera.pos.validVector(2)(0)), 2))
                                       
                                       + sqrt(pow((x(2).validVector(0)(0) - Front.kamera.pos.validVector(0)(0)), 2))
                                       + sqrt(pow((x(2).validVector(1)(0) - Front.kamera.pos.validVector(1)(0)), 2))
                                       + sqrt(pow((x(2).validVector(2)(0) - Front.kamera.pos.validVector(2)(0)), 2))
                                       
                                       >
                                         
                                         sqrt(pow((y(0).validVector(0)(0) - Front.kamera.pos.validVector(0)(0)), 2))
                                       + sqrt(pow((y(0).validVector(1)(0) - Front.kamera.pos.validVector(1)(0)), 2))
                                       + sqrt(pow((y(0).validVector(2)(0) - Front.kamera.pos.validVector(2)(0)), 2))
                                       
                                       + sqrt(pow((y(1).validVector(0)(0) - Front.kamera.pos.validVector(0)(0)), 2))
                                       + sqrt(pow((y(1).validVector(1)(0) - Front.kamera.pos.validVector(1)(0)), 2))
                                       + sqrt(pow((y(1).validVector(2)(0) - Front.kamera.pos.validVector(2)(0)), 2))
                                       
                                       + sqrt(pow((y(2).validVector(0)(0) - Front.kamera.pos.validVector(0)(0)), 2))
                                       + sqrt(pow((y(2).validVector(1)(0) - Front.kamera.pos.validVector(1)(0)), 2))
                                       + sqrt(pow((y(2).validVector(2)(0) - Front.kamera.pos.validVector(2)(0)), 2))
    )}
  )}
  
}

//object Filter {
//  def filter(array: Array[Array[VectorVer]]) = {
//    Front.kamera.rotation(
//  }
//}

object Projector {
  def project: Array[Array[(Double,Double)]] = {
    val data = Converter.read
    val rotation = Front.kamera.rotation
    var cameraPos = Front.kamera.pos
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
      newData.append(shape.toArray)
    }
    val d = newData.toArray
    val dReformated = Buffer[Array[(Double,Double)]]()
    for (shape <- d ) {
      var shapeReformated = Buffer[(Double,Double)]()
      for ( point <- shape) {
        val pointer = point.validVector
        val x: Double = (e(2)/pointer(2)(0))*pointer(0)(0) + e(0)
        val y: Double = (e(2)/pointer(2)(0))*pointer(1)(0) + e(1)
        val projected: (Double, Double) = (x,y)
        shapeReformated.append(projected)
      }
//      shapeReformated.foreach(println(_))
      dReformated.append(shapeReformated.toArray)
    }
    dReformated.toArray
  }
  
  def update(array:  Array[Array[VectorVer]]) : ( Array[Array[(Double,Double)]], Array[Array[VectorVer]] ) = {
    val holder = Buffer[Array[VectorVer]]()
    val rotation = Front.kamera.rotation
    var cameraPos = Front.kamera.pos
    for (row <- array) {
      val holderRow = Buffer[VectorVer]()
      for (vector <- row) {
        holderRow += 
                  new VectorVer(Array(
                    Array(
                      vector.validVector(0)(0) - cameraPos.validVector(0)(0)
                    ), Array(
                      vector.validVector(1)(0) - cameraPos.validVector(1)(0)
                    ), Array(
                      vector.validVector(2)(0) - cameraPos.validVector(2)(0)
                    )
                  ))
      }
      holder += holderRow.toArray
    }
    Front.kamera.resetPos
    cameraPos = Front.kamera.pos
    val data = holder.toArray
        
        
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
      newData.append(shape.toArray)
    }
    val d = newData.toArray
    val dReformated = Buffer[Array[(Double,Double)]]()
    for (shape <- d ) {
      var shapeReformated = Buffer[(Double,Double)]()
      for ( point <- shape) {
        val pointer = point.validVector
        val x: Double = (e(2)/pointer(2)(0))*pointer(0)(0) + e(0)
        val y: Double = (e(2)/pointer(2)(0))*pointer(1)(0) + e(1)
        val projected: (Double, Double) = (x,y)
        shapeReformated.append(projected)
      }
//      shapeReformated.foreach(println(_))
      dReformated.append(shapeReformated.toArray)
    }
    (dReformated.toArray, data)
  }
  
  
  
  
  
  
}