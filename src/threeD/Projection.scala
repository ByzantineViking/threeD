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
  // Uses the projecting formula mentioned in plan.
  def project(kamera: Camera): Array[Array[(Double,Double)]] = {
    // Required data
    val data = Converter.read
    val rotation = Front.kamera.rotation
    val cameraPos = Front.kamera.pos
    println(kamera.planePointToVector)
//    MathHelper.lineInterSectPlane(new VectorVer(Array(Array(0),Array(0),Array(2))), new VectorVer(Array(Array(0),Array(0),Array(-2))), kamera.planePointToVector, kamera.rotationVector.switchSign.normalize, pow(10, -6)) match {
//      case Some((fac, res)) => {
//        if (fac > 1.0) {
//        } else if (true) {
//          
//        } else {
//          
//        }
//      }
//      case None               =>
//    }
    
    // Selects only those whose normals point to the direction of the camera.
    // Dot product of the line from the camera to the triangle ( any point on triangle ) and normal of the triangle ( both normalized )
    var normals = MathHelper.normals(Converter.read)
    var partHiding = Buffer[Array[VectorVer]]()
    for (index <- data.indices) {
      if ((normals(index).normalize).dotProduct(data(index)(0).minus(kamera.pos).normalize) < 0.0) {
        partHiding.append(data(index))
      }
    }
    val partHidden: Array[Array[VectorVer]] = partHiding.toArray
     
    // Forming a matrix dependent of the current camera rotation.
    def transformationMatrix1 = new Matrix(Array(Array(1,0,0),Array(0, cos(rotation(0)), sin(rotation(0))),Array(0,-sin(rotation(0)),cos(rotation(0)))))
    def transformationMatrix2 = new Matrix(Array(Array(cos(rotation(1)),0,-sin(rotation(1))),Array(0, 1, 0),Array(sin(rotation(1)),0,cos(rotation(1)))))
    def transformationMatrix3 = new Matrix(Array(Array(cos(rotation(2)),sin(rotation(2)),0),Array(-sin(rotation(2)), cos(rotation(2)), 0),Array(0,0,1)))
    val transform = (transformationMatrix1.multiplyWithMatrix(transformationMatrix2)).multiplyWithMatrix(transformationMatrix3)
    
    // Handling the position and taking the above rotation into account.
    var newData = Buffer[Array[VectorVer]]()
    for (shape <- partHidden) {
      var shapes = Buffer[VectorVer]()
      for  (point <- shape) {
        shapes.append(transform.multiplyWithVector(point.minus(cameraPos)))
      }
      newData.append(shapes.toArray)
    }
    val dataInVectorVer = newData.toArray
    
    // The drawing distance. If the last value is negative, everything is inverted horizontally (across a vertical line),
    // and if the last value is positive, everything is inverted vertically (across a horizontal line).
    val e = kamera.planePoint
    
    // Rest of the math in the formula. Does the 2D planar projection.
    val finalProjection = Buffer[Array[(Double,Double)]]()
    for (shape <- dataInVectorVer) {
      var shapeReformated = Buffer[(Double,Double)]()
      for ( point <- shape) {
        val x: Double = e(2)/point.y* point.x + e(0)
        val y: Double = e(2)/point.y * point.z + e(1)
        val projected: (Double, Double) = (x,y)
        shapeReformated.append(projected)
      }
      finalProjection.append(shapeReformated.toArray)
    }
    finalProjection.toArray
  } // End of project-method.
  
  
  
  
}