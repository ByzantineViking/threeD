package threeD

// App
import scalafx.Includes._

import scalafx.application.JFXApp

import scalafx.scene.input.{KeyCode, KeyEvent, MouseEvent}

//import scalafx.event.ActionEvent

// To keep the mouse in the middle
import java.awt.Robot
import java.awt.{GraphicsEnvironment, GraphicsDevice}

import scalafx.scene.canvas._
//paths and fill
import scalafx.scene.{PerspectiveCamera, Group, SceneAntialiasing, Node}
import scalafx.scene.shape._

import scalafx.scene.Scene
import scalafx.scene.paint.Color
import javafx.scene.Cursor
//Basic
import scala.collection.mutable.Buffer

import scalafx.scene.text._

//import javafx.animation.AnimationTimer
import scalafx.animation.AnimationTimer

import scala.math.{sqrt, pow}



      
    
object Front extends JFXApp {
  
  
// FEEL FREE TO CHANGE THE FOLLOWING VALUES AS YOU PLEASE.
//--------------------------------------------------------------------------------//
        var playerSpeed  : Double   = 0.001
        val sensitivity  : Double   = 0.001
        
//--------------------------------------------------------------------------------//
    
        
    //Works as a reference point for, for example, ground level.
    var origo = new VectorVer(Array(Array(0.0),Array(0.0),Array(0.0)))
    

    Writer
    val kamera = new Camera
    val installData = Converter.read
    var data = Projector.update(installData)
    val root = new Group()
    val cam  = new PerspectiveCamera(true)
    val robot = new Robot()
    val centerPoint = (GraphicsEnvironment.getLocalGraphicsEnvironment().getCenterPoint.getX, GraphicsEnvironment.getLocalGraphicsEnvironment().getCenterPoint.getY)
    
    
    
    
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
    private var controlPressed: Boolean = false
    
    
    stage = new JFXApp.PrimaryStage {
      
      // Setting the size of the scene
      val base = 250
      val widthAspect = 4
      val heightAspect = 3
      width = widthAspect * base
      height = heightAspect * base
      
      
      scene = new Scene(root, widthAspect * base, heightAspect * base, depthBuffer = true, antiAliasing = SceneAntialiasing.Balanced) {
            cursor = Cursor.NONE
            
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
   
        //Drawing happens on the canvas, as it is quicker when dealing with having to re-draw everything every frame.
        val canvas = new Canvas(widthAspect * base, heightAspect * base)
        val gc = canvas.graphicsContext2D
        //Here starts the bread and butter of the animation, AnimationTimer.
        var lastTime = 0L
        
        //Related to jumping
        var zMomentum = 0.0
        var jumping = false
        val jumpingHeight = 0.5
        
        
        
        //to crouching
        var crouching = false
        var triggered = 0
        val timer: AnimationTimer = AnimationTimer(time => {
          if(lastTime>0) {
              gc.fill = Color.White
              gc.fillRect(0,0,widthAspect * base,heightAspect * base)
              
              origo   = Projector.updateOrigo(origo)
              data    = Projector.update(data._2)
              val delta = (time-lastTime)/1e9 // How much time has passed since last going through timer in seconds
              if (leftPressed) {
                  kamera.move(kamera.pos.validVector(0)(0) + playerSpeed * delta, 0, 0)          
              }
              if (rightPressed) {
                  kamera.move(kamera.pos.validVector(0)(0) - playerSpeed * delta, 0, 0) 
              }
              if (downPressed) {
                    kamera.move(0, 0, kamera.y - (playerSpeed * delta)) 
                    println(kamera.y)
                    println(triggered)
                    triggered += 1
//                    kamera.move(0,0,-0.01)
              }
              if (upPressed) {
                  kamera.move(0, 0, kamera.pos.validVector(2)(0) + playerSpeed * delta)
              }
              if (spacePressed) {
                 jumping = true
                 zMomentum = jumpingHeight
              }
              if (controlPressed) {
                 crouching = true
              } else {
                 crouching = false
              }
             
              
              
              //Jumping
              if (jumping) {
                kamera.move(0, kamera.pos.validVector(1)(0) + zMomentum * delta * 10 , 0)
                zMomentum = zMomentum - (jumpingHeight / 40.0)
                if (origo.z > 0) {
                  jumping = false
                  zMomentum = 0
                }
              }
              //Crouching
              if (crouching) {
                if(origo.z <= 0.25) {
                  kamera.move(0, kamera.pos.validVector(1)(0) - 10 * delta, 0)
                }
                
              } else {
                
              }
              
              val paths = Buffer[Path]()
              for (shape <- data._1) {
                if (shape.length == 3) {
                  gc.stroke = Color.Black
                  gc.strokePolygon(shape.map(x => (convertToCanvas(x)._1.toDouble, convertToCanvas(x)._2.toDouble) ) )
                  gc.fill = Color.Orange
                  gc.fillPolygon(shape.map(x => (convertToCanvas(x)._1.toDouble, convertToCanvas(x)._2.toDouble) ) )
                } else if (shape.length == 0) {
                  // empty row
                } else {
                  throw new NotTriangleException("Each element is not of the length of 3")
                }
              }
            }
          
          lastTime = time
        })
        timer.start
        
        onKeyPressed = (event: KeyEvent) => {
          event.code match {
            case KeyCode.W       =>  upPressed   = true
            case KeyCode.A       =>  leftPressed = true
            case KeyCode.S       =>  downPressed = true
            case KeyCode.D       =>  rightPressed= true
            case KeyCode.Space   =>  spacePressed= true
            case KeyCode.Control =>  controlPressed = true
            case KeyCode.Escape  =>  System.exit(1)
            case _ =>
          }
        }
        
        onKeyReleased = (event: KeyEvent) => {
          event.code match {
            case KeyCode.W       =>  upPressed   = false
            case KeyCode.A       =>  leftPressed = false
            case KeyCode.S       =>  downPressed = false
            case KeyCode.D       =>  rightPressed= false
            case KeyCode.Space   =>  spacePressed= false
            case KeyCode.Control =>  controlPressed = true
            case _               =>  
          }
        }
         
         
        onMouseMoved = (event: MouseEvent) => {
            mouseOldX = mousePosX
            mouseOldY = mousePosY
            mousePosX = event.sceneX
            mousePosY = event.sceneY
            mouseDeltaX = mousePosX - mouseOldX
            mouseDeltaY = mousePosY - mouseOldY
            kamera.rotate(mouseDeltaY * sensitivity , -mouseDeltaX * sensitivity , 0)
//            robot.mouseMove(centerPoint._1.toInt, centerPoint._2.toInt)
        }
        //Scene settings
        content = canvas
        title = "3D"
      }
   }
}


