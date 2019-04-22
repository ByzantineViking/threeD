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
  def project: (Array[Array[(Double,Double)]], Array[Array[VectorVer]]) = {
    // Required data
    val data: Array[Array[VectorVer]] = Converter.read
    val rotation = Camera.rotation
    val cameraPos = Camera.pos
    var planeCutData =  Buffer[Array[VectorVer]]()
   
    for (triangle <- data) {
//      // A call to clipping, it has an useful description.
      val planePointBack: VectorVer = Camera.pos.plus(new VectorVer(Array(Array(0), Array(0), Array(0))))
      val rotStraigth: VectorVer = Camera.rotationVector
      Sorter.clipping(triangle, planePointBack, rotStraigth)._1 match {
        case Some(array) => planeCutData.append(array)
        case None        => 
      }
      Sorter.clipping(triangle, planePointBack, rotStraigth)._2 match {
        case Some(array) => planeCutData.append(array)
        case None        => 
      }
    }
    val noTrianglesBehindCamera = planeCutData.toArray
    
    
      
    
    // Selects only those whose normals point to the direction of the camera.
    // Dot product of the line from the camera to the triangle ( any point on triangle ) and normal of the triangle ( both normalized )
    var normals = MathHelper.normals(noTrianglesBehindCamera)
    var partHiding = Buffer[Array[VectorVer]]()
    for (index <- noTrianglesBehindCamera.indices) {
      if ((normals(index).normalize).dotProduct(noTrianglesBehindCamera(index)(0).minus(Camera.pos).normalize) < 0.0) {
        partHiding.append(noTrianglesBehindCamera(index))
      }
    }
    val final3DCoordinates: Array[Array[VectorVer]] = partHiding.toArray
     
    // Forming a matrix dependent of the current camera rotation.
    def transformationMatrix1 = new Matrix(Array(Array(1,0,0),Array(0, cos(rotation(0)), sin(rotation(0))),Array(0,-sin(rotation(0)),cos(rotation(0)))))
    def transformationMatrix2 = new Matrix(Array(Array(cos(rotation(1)),0,-sin(rotation(1))),Array(0, 1, 0),Array(sin(rotation(1)),0,cos(rotation(1)))))
    def transformationMatrix3 = new Matrix(Array(Array(cos(rotation(2)),sin(rotation(2)),0),Array(-sin(rotation(2)), cos(rotation(2)), 0),Array(0,0,1)))
    val transform = (transformationMatrix1.multiplyWithMatrix(transformationMatrix2)).multiplyWithMatrix(transformationMatrix3)
    
    // Handling the position and taking the above rotation into account.
    var newData = Buffer[Array[VectorVer]]()
    for (shape <- final3DCoordinates) {
      var shapes = Buffer[VectorVer]()
      for  (point <- shape) {
        shapes.append(transform.multiplyWithVector(point.minus(cameraPos)))
      }
      newData.append(shapes.toArray)
    }
    val dataInVectorVer = newData.toArray
    
    // The drawing distance. If the last value is negative, everything is inverted horizontally (across a vertical line),
    // and if the last value is positive, everything is inverted vertically (across a horizontal line).
    val e = Camera.planePoint
    
    // Rest of the math in the formula. Does the 2D planar projection.
    val finalProjection = Buffer[Array[(Double,Double)]]()
    for (shape <- dataInVectorVer) {
      var shapeReformated = Buffer[(Double,Double)]()
      for ( point <- shape) {
        val x: Double = (e(2)/point.y)* point.x + e(0)
        val y: Double = (e(2)/point.y) * point.z + e(1)
        val projected: (Double, Double) = (x,y)
        shapeReformated.append(projected)
      }
      finalProjection.append(shapeReformated.toArray)
    }
    (finalProjection.toArray, final3DCoordinates)
  } // End of project-method.
  
  
  
  
}