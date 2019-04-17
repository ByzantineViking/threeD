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
  
        var playerSpeed  : Double   = 1
        val sensitivity  : Double   = 2 * pow(10, -3)
        var stamina      : Double   = 5
        // Recoveral time in seconds.
        var recoveralTime: Double = 1.0
        var doubleJump   : Boolean = true
//--------------------------------------------------------------------------------//
        
        // Setting the size of the scene
        val base = 250
        val widthAspect = 4
        val heightAspect = 3
        
//--------------------------------------------------------------------------------//
        
        
    // Setting up
    Writer
    val kamera = new Camera
    val installData = Converter.read
    var data = Projector.project
    
    
    // ScalaFx specific requirements
    val root = new Group()
    val robot = new Robot()
        
    // Center of the whole screen, independent of monitor size.
    val centerPoint: (Double, Double) = (GraphicsEnvironment.getLocalGraphicsEnvironment().getCenterPoint.getX, GraphicsEnvironment.getLocalGraphicsEnvironment().getCenterPoint.getY)
    
    
    
    
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
    private var shiftPressed: Boolean = false
    
    var userInput = true
    
    stage = new JFXApp.PrimaryStage {
         
      // Creating the size of the stage. Scene is set to same size.
      width = widthAspect * base
      height = heightAspect * base
      
      
      scene = new Scene(root, widthAspect * base, heightAspect * base, depthBuffer = true, antiAliasing = SceneAntialiasing.Balanced) {
            cursor = Cursor.NONE
            
            
            
            // Helper function to rebase the coordinate system to the screen.
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
        var velocity = 0.0
        var jumping = false
        // Double jumping
        var jumped = false
        var jumpedTwice = false
        var timeFromJump = 0.0
        
        var truePlayerSpeed: Double = playerSpeed
        
        //  Sprinting
        var sprinting = false
        
        //  Stamina  
        //  It re-charges all the time, but if it runs out, a waiting period of recoveralTime (in settings) is gone through,
        //  after which stamina starts to re-charge. If you have ran out, you must wait till it's over half before running or jumping again.
        var staminaLeft = stamina
        var recovering = false
        var replenishing = false
        var trueRecoveralTime = recoveralTime
        
        //  Flags for crouching
        var crouching = false
        var gettingUp = false

        
//-------------------------------------------------------------------------------------------//
        // Timer is the loop inside of which the whole application runs.
        val timer: AnimationTimer = AnimationTimer(time => {
          if(lastTime>0) {
              // To smooth out the motion, player moves according to the tickrate.
              val delta = (time-lastTime)/1e9 // How much time has passed since last going through timer in seconds
              timeFromJump += delta
              
              // Reacts to changes in rotation and position.
              data = Projector.project
              
              // Starts the canvas as a white background.
              gc.fill = Color.White
              gc.fillRect(0,0,widthAspect * base,heightAspect * base)
              
              
        //-------------------------------------------------------------------------------------------//
              
              // Stamina
              // Needed for running and jumping
              if (staminaLeft <= 0) {
                staminaLeft = 0
                recovering = true 
              } else if (staminaLeft >= stamina/2) {
                replenishing = false
                recovering   = false
              }
              if (recovering) {
                if (replenishing) {
                  staminaLeft += 0.01
                } else {
                  trueRecoveralTime -= delta
                  if (trueRecoveralTime <= 0) {
                    replenishing = true
                    trueRecoveralTime = recoveralTime
                  }
                }
              } else if (staminaLeft < stamina) {
                staminaLeft += 0.03
              }
              
              
              // Jumping
              if (spacePressed && kamera.z == 0 && !recovering) {
                 jumping = true
                 jumped = true
                 velocity = 4.12
                 staminaLeft = 0.0
                 timeFromJump = 0.0
              }
              if (spacePressed && doubleJump && kamera.z >= 0.4 && timeFromJump >= 0.4 && jumped && !jumpedTwice) {
                 println("two")
                 jumping = true
                 jumpedTwice = true
                 velocity = 4.12
                 staminaLeft = 0.0
              }
              if (velocity == 0.0) {
                jumped = false
                jumpedTwice = false
              }
              
              if (kamera.z < 0 && jumping) {
                  jumping = false
                  velocity = 0.0
                  kamera.move(0,-kamera.z,0)
                }
              if (jumping) {
                kamera.move(0, velocity * delta, 0)
                velocity -= 9.81 * delta
              }
              
              // Sprinting
              if (shiftPressed && kamera.z == 0 && !recovering) {
                sprinting = true
              } else {
                sprinting = false
              }
              if (sprinting) {
                staminaLeft -= 0.04
              }
              
              // Crouching
              if (controlPressed && (kamera.z <= 0)) {
                 crouching = true
                 gettingUp = false
              } else {
                 crouching = false
              }
              
              if (crouching) {
                if (kamera.z > -0.25) {
                  kamera.move(0, -0.8 * delta, 0)
                } else if (kamera.z < -0.25) {
                  kamera.moveTo(kamera.x, -0.25, kamera.y)
                }
              } else if (gettingUp) {
                if (kamera.z < 0) {
                  kamera.move(0, 1.2 * delta, 0)
                } else if (kamera.z > 0) {
                  kamera.moveTo(kamera.x, 0.0, kamera.y)
                  gettingUp = false
                } else {
                  gettingUp = false
                }
              }
              
         //-------------------------------------------------------------------------------------------//     
              
              // Determining the speed of player
              if (crouching) {
                truePlayerSpeed = playerSpeed / 2.5
              } else if (gettingUp) {
                truePlayerSpeed = playerSpeed / 3.0
              } else if (jumping && sprinting) {
                truePlayerSpeed = playerSpeed * 0.9
              } else if (sprinting) {
                truePlayerSpeed = playerSpeed * 1.3
              } else if (recovering) {
                truePlayerSpeed = playerSpeed * 0.85
              } else {
                truePlayerSpeed = playerSpeed
              }
              
              
              
              if (leftPressed) {
                  kamera.move(truePlayerSpeed * delta, 0, 0)          
              }
              if (rightPressed) {
                  kamera.move(-truePlayerSpeed * delta, 0, 0) 
              }
              if (downPressed) {
                   kamera.move(0, 0, - truePlayerSpeed * delta)
              }
              if (upPressed) {
                  kamera.move(0, 0,truePlayerSpeed * delta)
              }
              
              
              
              
              
              
              
              
         //-------------------------------------------------------------------------------------------//     
              // Drawing the things with graphicsContext2D
              
              val paths = Buffer[Path]()
              for (shape <- data) {
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
              
              
              
              
              
            } // End of the if statement which prevents drawing when application is opening.
            lastTime = time
        })    // End of timer
        timer.start
        // Timer is the loop inside of which the whole application runs.
        
//-------------------------------------------------------------------------------------------//
        // All the input listeners.
        
        
        onKeyPressed = (event: KeyEvent) => {
          event.code match {
            case KeyCode.W       =>  upPressed        = true
            case KeyCode.A       =>  leftPressed      = true
            case KeyCode.S       =>  downPressed      = true
            case KeyCode.D       =>  rightPressed     = true
            case KeyCode.Space   =>  spacePressed     = true
            case KeyCode.Control =>  controlPressed   = true
            case KeyCode.Shift   =>  shiftPressed     = true
            case KeyCode.Escape  =>  System.exit(1)
            case _ =>
          }
        }
        
        onKeyReleased = (event: KeyEvent) => {
          event.code match {
            case KeyCode.W       =>  upPressed          = false
            case KeyCode.A       =>  leftPressed        = false
            case KeyCode.S       =>  downPressed        = false
            case KeyCode.D       =>  rightPressed       = false
            case KeyCode.Space   =>  spacePressed       = false
            // Releasing control starts getting up process.
            case KeyCode.Control =>{
                                     controlPressed     = false
                                     gettingUp          = true
            }
            // Similarly releasing shift starts recoveral.
            case KeyCode.Shift   =>{
                                     shiftPressed       = false
                                     recovering         = true
                                     sprinting          = false
            }
            case _               =>  
          }
        }
        
        onMouseMoved = (event: MouseEvent) => {
            mouseDeltaX = event.screenX - centerPoint._1
            mouseDeltaY = event.screenY - centerPoint._2
            kamera.rotate(mouseDeltaY * sensitivity , -mouseDeltaX * sensitivity , 0)
            robot.mouseMove(centerPoint._1.toInt, centerPoint._2.toInt)
        }
        onMouseDragged = (event: MouseEvent) => {
            mouseDeltaX = event.screenX - centerPoint._1
            mouseDeltaY = event.screenY - centerPoint._2
            kamera.rotate(mouseDeltaY * sensitivity , -mouseDeltaX * sensitivity , 0)
            robot.mouseMove(centerPoint._1.toInt, centerPoint._2.toInt)
        }

 //-------------------------------------------------------------------------------------------//
        
        //Scene settings.
        content = canvas
        title = "3D"
      }  // End of scene
   }     // End of stage
}        // End of App
// As seen from the ending above, everything is done inside the scene, on the canvas, within the timer loop.

