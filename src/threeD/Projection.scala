package threeD
import scala.collection.mutable.Buffer
import scala.math._


// Converter takes the data, and converts it to right form Array[Array[VectorVer]] for projecting.
object Converter {
  def convertCSV: Array[Array[VectorVer]] = {
    var data:  Array[Array[Double]] = CSVReader.readCSV
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
  def convertObject: Array[Array[VectorVer]] = {
    var data:  Array[Array[Double]] = ObjectReader.readObj
    var holder = Buffer[Double]()
    val triangledData = Buffer[Array[Double]]()
    for (point <- data) {
      if (holder.length < 9) {
        holder.append(point(0), point(1), point(2))
      } else {
        val h = holder.toArray
//        triangledData.append(h)
        triangledData.append(Array(h(6),h(7),h(8),h(3),h(4),h(5),h(0),h(1),h(2)))
        holder.clear()
      }
    }
    
    
    var vectors = Buffer[Array[VectorVer]]()
    for (line <- triangledData.toArray) {
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


/**
 *  Main algorithm. Filters and projects the triangles.
 */
object Projector {
  // Uses the projecting formula mentioned in plan.
  def project: (Array[Array[(Double,Double)]], Array[Array[VectorVer]]) = {
    // Required data
    var data : Array[Array[VectorVer]] = Array[Array[VectorVer]]()
    if (Front.fileIsCSV) {
      data = Converter.convertCSV
    } else {
      data = Converter.convertObject    
    }
    val rotation = Camera.rotation
    val cameraPos = Camera.pos
    
    /**
     * Limited functionality.
     */
    if(Front.clippingEnabled) {
      data = Intersection.intersectWithCameraPlane(data)
    }
    
    // Selects only those whose normals point to the direction of the camera.
    // Dot product of the line from the camera to the triangle ( any point on triangle ) and normal of the triangle ( both normalized )
    var normals = MathHelper.normals(data)
    var partHiding = Buffer[Array[VectorVer]]()
    for (index <- data.indices) {
      if ((normals(index).normalize).dotProduct(data(index)(0).minus(Camera.pos).normalize) < 0.0) {
        partHiding.append(data(index))
      }
    }
    val final3DCoordinates: Array[Array[VectorVer]] = partHiding.toArray
     
    // Forming a matrix dependent of the current camera rotation.
    def transformRotationY = new Matrix(Array(Array(1,0,0),Array(0, cos(rotation(0)), sin(rotation(0))),Array(0,-sin(rotation(0)),cos(rotation(0)))))
    def transformRotationX = new Matrix(Array(Array(cos(rotation(1)),0,-sin(rotation(1))),Array(0, 1, 0),Array(sin(rotation(1)),0,cos(rotation(1)))))
    
    // The following rotation is in the leaning to the sides- direction, as in rocking a boat. Enable if you want to enable rotation like that.
    def transformRotationRocking = new Matrix(Array(Array(cos(rotation(2)),sin(rotation(2)),0),Array(-sin(rotation(2)), cos(rotation(2)), 0),Array(0,0,1)))
    // Matrix calculations.
    val transform = (transformRotationY.multiplyWithMatrix(transformRotationX)).multiplyWithMatrix(transformRotationRocking)
    
    // The drawing distance. If the last value is negative, everything is inverted horizontally (across a vertical line),
    // and if the last value is positive, everything is inverted vertically (across a horizontal line).
    val e = Camera.planePoint
    
    // Handling the position and taking the above rotation into account.
    // Also does the 2D planar projection.
    val finalProjection = Buffer[Array[(Double,Double)]]()
    for (shape <- final3DCoordinates) {
      val shapeReformated = Buffer[(Double,Double)]()
      for  (point <- shape) {
        val transformedIn3D: VectorVer = (transform.multiplyWithVector(point.minus(cameraPos)))
        val x: Double = (e(2)/transformedIn3D.y)* transformedIn3D.x + e(0)
        val y: Double = (e(2)/transformedIn3D.y) * transformedIn3D.z + e(1)
        val projected: (Double, Double) = (x,y)
        shapeReformated.append(projected)
      }
      finalProjection.append(shapeReformated.toArray)
    }
    (finalProjection.toArray, final3DCoordinates)
  } // End of project-method.
  
  
  
  
}