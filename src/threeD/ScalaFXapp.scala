package threeD

// App
import scalafx.Includes._

import scalafx.application.JFXApp

import scalafx.scene.input.{KeyCode, KeyEvent, MouseEvent}

//import scalafx.event.ActionEvent

// To keep the mouse in the middle
import java.awt.Robot

import scalafx.scene.canvas._
//paths and fill
import scalafx.scene.{PerspectiveCamera, Group, SceneAntialiasing, Node}
import scalafx.scene.shape._

import scalafx.scene.Scene
import scalafx.scene.paint.Color
//Basic
import scala.collection.mutable.Buffer

import scalafx.scene.text._

//import javafx.animation.AnimationTimer
import scalafx.animation.AnimationTimer

import scala.math.{sqrt, pow}

object Front extends JFXApp {
    Writer
    val kamera = new Camera
    val installData = Converter.read
    var data = Projector.update(installData)
    val root = new Group()
    val cam  = new PerspectiveCamera(true)
    private var mousePosX: Double = .0
    private var mousePosY: Double = .0
    private var mouseOldX: Double = .0
    private var mouseOldY: Double = .0
    private var mouseDeltaX: Double = .0
    private var mouseDeltaY: Double = .0
    
    
    private var upPressed: Boolean = false
    private var downPressed: Boolean = false
    private var rightPressed: Boolean = false
    private var leftPressed: Boolean = false
    private var spacePressed: Boolean = false
    
    
    stage = new JFXApp.PrimaryStage {
      
      // Setting the size of the scene
      val base = 250
      val widthAspect = 4
      val heightAspect = 3
      
      width = widthAspect * base
      height = heightAspect * base
      
      
      scene = new Scene(root, widthAspect * base, heightAspect * base, depthBuffer = true, antiAliasing = SceneAntialiasing.Balanced) {
        
            
            def convertToCanvas(x : (Double, Double)): (Int, Int) = {
                  ((((x._1 + 1.0)/2.0) * widthAspect * base).toInt, ((x._2 + 1)/2 * heightAspect * base).toInt)
              }
        
            def drawTriangles(A: (Double,Double), B: (Double,Double), C:(Double,Double)) = {
              
              val a = convertToCanvas(A)
              val b = convertToCanvas(B)
              val c = convertToCanvas(C)
              
              
              val path = new Path
              path.elements += MoveTo(a._1, a._2)
              path.elements += LineTo(b._1,b._2)
              path.elements += LineTo(c._1,c._2)
              path.elements += new ClosePath
              path.fill = Color.Yellow
//              path.stroke = Color.Yellow
              path
            }
        
//        def drawWall(A: (Double,Double), B: (Double,Double), C:(Double,Double), D: (Double, Double)) = {
//              def convertToCanvas(x : (Double, Double)): (Int, Int) = {
//                  ((((x._1 + 1.0)/2.0) * widthAspect * base).toInt, ((x._2 + 1)/2 * heightAspect * base).toInt)
//              }
//              val a = convertToCanvas(A)
//              val b = convertToCanvas(B)
//              val c = convertToCanvas(C)
//              val d = convertToCanvas(D)
//              
//              
//              
//              
//            }
        
         
    /**
     * @configuration:
     */
        
        val playerSpeed = 0.2
        val sensitivity = 0.001
        
        
        val canvas = new Canvas(widthAspect * base, heightAspect * base)
        val gc = canvas.graphicsContext2D
        var lastTime = 0L
//        var enemies = List(Circle(10,10,10))
//        val enemySpeed = 150
//        val playerSpeed = 170
//        val player= Circle(300,300,20)
//        
//        player.fill = Color.Green
        val timer: AnimationTimer = AnimationTimer(time => {
          if(lastTime>0) {
            gc.fill = Color.White
            gc.fillRect(0,0,widthAspect * base,heightAspect * base)
            data = Projector.update(data._2)
            val delta = (time-lastTime)/1e9 // How much time has passed since last going through timer in seconds
//            for (e<- enemies) {
//              val dx = player.centerX.value - e.centerX.value
//              val dy = player.centerY.value - e.centerY.value
//              val dist = math.sqrt(dx*dx+dy*dy)
//              if (dist< e.radius.value + player.radius.value) {
//                content += new Text(250, 300,"YOU DIED")
//                timer.stop
//              }
//              e.centerX = e.centerX.value + dx/dist*enemySpeed*delta
//              e.centerY = e.centerY.value + dy/dist*enemySpeed*delta
//            }
            if (leftPressed) {
                kamera.move(kamera.pos.validVector(0)(0) + playerSpeed * delta, 0, 0)          
            }
            if (rightPressed) {
                kamera.move(kamera.pos.validVector(0)(0) - playerSpeed * delta, 0, 0) 
            }
            if (downPressed) {
                kamera.move(0, 0, kamera.pos.validVector(0)(0) - playerSpeed * delta) 
            }
            if (upPressed) {
                kamera.move(0, 0, kamera.pos.validVector(2)(0) + playerSpeed * delta)
            }
          }
          
//          gc.fill = Color.Red
//          gc.fillOval(300, 300, 20, 20)
          
          lastTime = time
          val paths = Buffer[Path]()
          for (shape <- data._1) {
            if (shape.length == 3) {
              gc.fill = Color.Orange
              gc.stroke = Color.Black
              gc.strokePolygon(shape.map(x => (convertToCanvas(x)._1.toDouble, convertToCanvas(x)._2.toDouble) ) )
              gc.fillPolygon(shape.map(x => (convertToCanvas(x)._1.toDouble, convertToCanvas(x)._2.toDouble) ) )
           
              
//            } else if (shape.length == 4) {
//              walls.append(drawWall(shape(0),shape(1),shape(2),shape(3))))
            } else if (shape.length == 0) {
              // empty row
            } else {
              throw new NotTriangleException("Each element is not of the length of 3")
            }
          }
          
          
        })
        timer.start
        
        
        canvas.onMouseMoved = (event: MouseEvent) => {
//          kamera.rotate(event.x * sensitivity, 0, 0)
//          kamera.rotate(0, event.y * sensitivity, 0)
          println(kamera.rotation)
        }
        
        
        onKeyPressed = (event: KeyEvent) => {
          event.code match {
            case KeyCode.W => {
              upPressed = true
            }
            case KeyCode.A => {
              leftPressed = true
            }
            case KeyCode.S => {
              downPressed = true
            }
            case KeyCode.D => {
              rightPressed = true
            }
            case KeyCode.Space => {
              spacePressed = true
            }
            case _ =>
          }
        }
        
        onKeyReleased = (event: KeyEvent) => {
          event.code match {
            case KeyCode.W     =>  upPressed   = false
            case KeyCode.A     =>  leftPressed = false
            case KeyCode.S     =>  downPressed = false
            case KeyCode.D     =>  rightPressed= false
            case KeyCode.Space =>  spacePressed= false
            case _             =>  
          }
        }
        
        
//        val walls = Buffer[Rectangle]()
        //Actually setting the scene
        
        
        content = canvas
        
        title = "3D"
        
      }
      handleMouse(scene())
   }
    
   private def handleMouse(scene: Scene) {
    scene.onMousePressed = (me: MouseEvent) => {
      mousePosX = me.sceneX
      mousePosY = me.sceneY
      mouseOldX = me.sceneX
      mouseOldY = me.sceneY
    }
    scene.onMouseMoved = (me: MouseEvent) => {
      mouseOldX = mousePosX
      mouseOldY = mousePosY
      mousePosX = me.sceneX
      mousePosY = me.sceneY
      mouseDeltaX = mousePosX - mouseOldX
      mouseDeltaY = mousePosY - mouseOldY
      val modifier = if (me.isControlDown) 0.1 else if (me.isShiftDown) 10 else 1.0
      val modifierFactor = 0.1
      if (me.isPrimaryButtonDown) {
        
      } else {
        kamera.rotate(mouseDeltaX, mouseDeltaY, 0)
      }
    }
  }
}


