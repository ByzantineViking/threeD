package threeD

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.paint.Color._

//paths and fill
import scalafx.scene.control._
import scalafx.scene.shape._
import scala.collection.mutable.Buffer
object Front extends JFXApp {
    val camera = new Camera
    val data = Projector.project
//    println(data.length)
//    for (triangle <- data) {
//      println(triangle.length)
//      for (element <- triangle) {
//        println(element)
//      }
//    }
    stage = new JFXApp.PrimaryStage {
//    def convertCoordinates(triangle: Array[Array[Double]]) = {
//      for (row <- triangle) {
//        for (element <- row) {
//      }
//    }
      
    def convertToCanvas(x : (Double, Double)): (Int, Int) = {
      ((((x._1 + 1.0)/2.0) * widthAspect * base).toInt, ((x._2 + 1)/2 * heightAspect * base).toInt)
    }
    
    val test : Array[(Double, Double)] = Array((-0.5, -0.5),(0.5,0), (0, -0.5))
    title.value = "3D"
    val base = 250
    val widthAspect = 4
    val heightAspect = 3
    width = widthAspect * base
    height = heightAspect * base
    scene = new Scene {
      
//      val svg = new SVGPath
//      svg.content = "M10, 10, L10,190 C190,190 300, 10 10, 10"
    def drawTriangles(A: (Double,Double), B: (Double,Double), C:(Double,Double)) = {
        val path = new Path
        val a = convertToCanvas(A)
        val b = convertToCanvas(B)
        val c = convertToCanvas(C)
//        println(a.toString + b.toString + c.toString)
        path.elements += MoveTo(a._1, a._2)
        path.elements += LineTo(b._1,b._2)
        path.elements += LineTo(c._1,c._2)
        path.elements += new ClosePath
        path.fill = Yellow
        path
      }
    val paths = Buffer[Path]()
    for (triangle <- data) {
      if (triangle.length == 3) {
        paths.append(drawTriangles(triangle(0),triangle(1),triangle(2)))
      } else if (triangle.length == 0) {
        //There is empty lines in between
//        println("Empty line")
      } else {
        throw new NotTriangleException("Each element is not of the length of 3")
      }
    }
      
      
      
//      val path = new Path
//      path.elements += MoveTo(200, 200)
//      path.elements += LineTo(200,300)
//      path.elements += LineTo(300,300)
//      path.elements += VLineTo(200)
//      path.elements += new ClosePath
//      path.fill = Grey
//      fill = LightGreen
//      val rectangle = new Rectangle {
//          x = 25
//          y = 40
//          width = 100
//          height = 100
//          fill <== when (hover) choose Green otherwise Red
//      }
    
      fill = Black
      content = paths
        
    }
  }
}
