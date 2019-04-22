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

import scala.math._



      
/**
 * The application window. Everything regarding ScalaFX, and various game mechanics.  
 */
object Front extends JFXApp {
  
  
/**
 *  SETTINGS
 * 
 *  FEEL FREE TO CHANGE THE FOLLOWING VALUES AS YOU PLEASE.
 *  
 *  ALSO CREATE OWN WORLDS IN "CSVMaker"!
 */
//--------------------------------------------------------------------------------//
  
       
  
        var playerSpeed  : Double       = 1
        val sensitivity  : Double       = 2 * pow(10, -3)
        val stamina      : Double       = 5
        // Recoveral time in seconds.
        val recoveralTime: Double       = 1.0
        val doubleJump   : Boolean      = true
        val endlessStamina: Boolean     = false
        val flying       : Boolean      = true
        
//--------------------------------------------------------------------------------//
        
         // Clips the triangle behind camera, but decreases performance
        val clippingEnabled             = false
        
//--------------------------------------------------------------------------------//
  
        // Setting the size of the scene
        val base = 250
        val widthAspect = 4
        val heightAspect = 3
        
//--------------------------------------------------------------------------------//
        // For loading a CSV file
        val fileName = "data.csv"
        
        // For loading a object file
        val objectName = "temple3.obj"
        
        // If false, read object, if true, read CSV
        val fileIsCSV = false
//--------------------------------------------------------------------------------//
// END OF SETTINGS. 
        
        
    // Setting up
    Writer
    
    var fullData = Projector.project
    var data = fullData._1
    var distances: Array[Double] = fullData._2.map(triangle => MathHelper.triangleDistanceFromCameraPoint(triangle))
    
    
    
    // ScalaFx specific requirements
    val root = new Group()
        
        
        
    // Robot moves the mouse back to the center of the screen. If the processor processes all the frames, the mouse stays within the window.
    val robot = new Robot()
    // Center of the whole screen, independent of monitor size.
    val centerPoint: (Int, Int) = (GraphicsEnvironment.getLocalGraphicsEnvironment().getCenterPoint.getX.toInt, GraphicsEnvironment.getLocalGraphicsEnvironment().getCenterPoint.getY.toInt)
    
    
    // Take the amount the mouse has moved each frame.
    private var mouseDeltaX: Double = .0
    private var mouseDeltaY: Double = .0
    
    // Enable multiple keys to be pressed at once.
    private var upPressed: Boolean = false
    private var downPressed: Boolean = false
    private var rightPressed: Boolean = false
    private var leftPressed: Boolean = false
    private var leanRight: Boolean = false
    private var leanLeft: Boolean = false
    private var spacePressed: Boolean = false
    private var controlPressed: Boolean = false
    private var shiftPressed: Boolean = false
    private var zoomingIn: Boolean = false
    private var zoomingOut: Boolean = false
    
//-------------------------------------------------------------------------------------------//
    
    stage = new JFXApp.PrimaryStage {
         
      // Creating the size of the stage. Scene is set to same size.
      width = widthAspect * base
      height = heightAspect * base
      
      
      scene = new Scene(root, widthAspect * base, heightAspect * base, depthBuffer = true, antiAliasing = SceneAntialiasing.Balanced) {
            cursor = Cursor.NONE
            
        //Drawing happens on the canvas, as it is quicker when dealing with having to re-draw everything every frame.
        val canvas = new Canvas(widthAspect * base, heightAspect * base)
        val gc = canvas.graphicsContext2D

//-----------------------------------------------------------------------------------------------------------------------------------------//
        
        // Flags, counters, and keepers of game mechanics.
        
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

        
        var leaningUp = false
         
//-----------------------------------------------------------------------------------------------------------------------------------------//
        
        // Here starts the bread and butter of the animation, AnimationTimer.
        var lastTime = 0L
        // Timer is the loop inside of which the whole application runs.
        val timer: AnimationTimer = AnimationTimer(time => {
          if(lastTime>0) {
              // To smooth out the motion, player moves according to the tickrate.
              val delta = (time-lastTime)/1e9 // How much time has passed since last going through timer in seconds
              timeFromJump += delta
              // Reacts to changes in rotation and position.
              fullData =  Projector.project
              data = fullData._1
              distances = fullData._2.map(triangle => MathHelper.triangleDistanceFromCameraPoint(triangle))
              
              Camera.roundRotation
              
              // Starts the canvas as a white background.
              gc.fill = Color.White
              gc.fillRect(0,0,widthAspect * base,heightAspect * base)
              
        //-------------------------------------------------------------------------------------------//
              
              // Game mechanics.
              
              // Stamina
              // Needed for running and jumping
              if (!endlessStamina) {
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
              }
              
              
              // Jumping
              if (spacePressed && ((Camera.z == 0 && !recovering) || flying)) {
                 jumping = true
                 jumped = true
                 velocity = 4.12
                 if (endlessStamina || flying) {
                   // No effect to stamina
                 } else {
                   staminaLeft = 0.0
                 }
                 timeFromJump = 0.0
              }
              // Double jump. Not possible while flying mode enabled.
              if (spacePressed && doubleJump && Camera.z >= 0.4 && timeFromJump >= 0.4 && jumped && !jumpedTwice && !flying) {
                 jumping = true
                 jumpedTwice = true
                 velocity = 4.12
                 if (!endlessStamina) {
                   staminaLeft = 0.0
                 }
              }
              if (velocity == 0.0) {
                jumped = false
                jumpedTwice = false
              }
              
              if (Camera.z < 0 && jumping) {
                  jumping = false
                  velocity = 0.0
                  Camera.move(0,-Camera.z,0)
                }
              if (jumping) {
                Camera.move(0, velocity * delta, 0)
                velocity -= 9.81 * delta
              }
              // Sprinting
              if (shiftPressed && Camera.z == 0 && (!recovering || endlessStamina)) {
                sprinting = true
              } else {
                sprinting = false
              }
              if (sprinting && !endlessStamina) {
                staminaLeft -= 0.04
              }
              
              
              if (leaningUp) {
                if (Camera.leaning > 0.0) {
                  Camera.rotate(0, 0, -20 * sensitivity  / (2*Pi))
                  closeEnough
                } else if (Camera.leaning < 0.0) {
                  Camera.rotate(0, 0, 20 * sensitivity  / (2*Pi))
                  closeEnough
                } else {
                  leaningUp = false
                }
              }
              def closeEnough: Unit = {
                if (Camera.leaning.abs < 0.01) {
                  Camera.resetLeaning
                }
              }
              
              // Crouching
              if (controlPressed && (Camera.z <= 0)) {
                 crouching = true
                 gettingUp = false
              } else {
                 crouching = false
              }
              
              if (crouching) {
                if (Camera.z > -0.25) {
                  Camera.move(0, -0.8 * delta, 0)
                } else if (Camera.z < -0.25) {
                  Camera.moveTo(Camera.x, -0.25, Camera.y)
                }
                lean
              } else if (gettingUp) {
                if (Camera.z < 0) {
                  Camera.move(0, 1.2 * delta, 0)
                } else if (Camera.z > 0) {
                  Camera.moveTo(Camera.x, 0.0, Camera.y)
                  gettingUp = false
                } else {
                  gettingUp = false
                }
              }
              def lean: Unit = {
                if (leanRight && !leanLeft && !leaningUp && Camera.leaning < 0.5) {
                    Camera.rotate(0, 0, 10 * sensitivity  / (2*Pi))
                 } else if (leanLeft && !leanRight && !leaningUp && Camera.leaning > -0.5) {
                    Camera.rotate(0, 0, -10 * sensitivity  / (2*Pi))
                 }
              }
              
              
         //-------------------------------------------------------------------------------------------//     
              
              // Determining the speed of player
              if (flying && jumping) {
                truePlayerSpeed = playerSpeed * 1.6
              } else if (crouching) {
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
              
              // If setting velocity to 0, the jumping ends
              
              // Moving.
              
              if (leftPressed) {
                  Camera.move(truePlayerSpeed * delta * cos(Camera.rotation(1)), 0, -truePlayerSpeed * delta * sin(Camera.rotation(1)))
              }
              if (rightPressed) {
                  Camera.move(-truePlayerSpeed * delta * cos(Camera.rotation(1)), 0, truePlayerSpeed * delta * sin(Camera.rotation(1))) 
              }
              if (downPressed) {
                   Camera.move(-truePlayerSpeed * delta * sin(Camera.rotation(1)), 0, -truePlayerSpeed * delta * cos(Camera.rotation(1))) 
              }
              if (upPressed) {
                  Camera.move(truePlayerSpeed * delta * sin(Camera.rotation(1)), 0, truePlayerSpeed * delta * cos(Camera.rotation(1)))  
              }
              
              
              // Zooming.
              if (zoomingIn && !zoomingOut) {
                Camera.zoomIn
              } else if (!zoomingIn && zoomingOut) {
                Camera.zoomOut
              }
              
              
         //-------------------------------------------------------------------------------------------//     
              // Drawing the things with graphicsContext2D
              
              // Helper function to rebase the coordinate system to the screen.
              def convertToCanvas(x : (Double, Double)): (Int, Int) = {
                    ((((x._1 + 1.0)/2.0) * widthAspect * base).toInt, ((x._2 + 1)/2 * heightAspect * base).toInt)
              }
              val max: Double = {
                if (distances.isEmpty) {
                  -1
                } else {
                  distances.max
                }
              }
              for (index <- data.indices) {
                if (data(index).length == 3) {
                  gc.stroke = Color.Black
                  gc.strokePolygon(data(index).map(x => (convertToCanvas(x)._1.toDouble, convertToCanvas(x)._2.toDouble) ) )
                  
                  // ENABLE ONE OF FOLLOWING
                  
                  // Darkness scales relative to the farthest object.
//                  gc.fill = Color.FireBrick.deriveColor(0, 1, 1.2- distances(index)/max, 1)
                  
                  // Darkness scales relative to distance, personal favourite
                  gc.fill = Color.FireBrick.deriveColor(0, 1, 1 - distances(index)/20.0, 1)
                  
                  gc.fillPolygon(data(index).map(x => (convertToCanvas(x)._1.toDouble, convertToCanvas(x)._2.toDouble) ) )
                } else if (data(index).length == 0) {
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
            case KeyCode.E       =>  leanRight        = true
            case KeyCode.Q       =>  leanLeft         = true
            case KeyCode.Space   =>  spacePressed     = true
            case KeyCode.Control =>  controlPressed   = true
            case KeyCode.Shift   =>  shiftPressed     = true
            case KeyCode.Plus    =>  zoomingIn        = true
            case KeyCode.Minus   =>  zoomingOut       = true
            case KeyCode.Z       =>  Camera.defaultZoom
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
            case KeyCode.E       =>  {
                                     leanRight          = false
                                     leaningUp          = true
            }
            case KeyCode.Q       =>  {
                                     leanLeft          = false
                                     leaningUp          = true
            }
            case KeyCode.Space   =>  spacePressed       = false
            // Releasing control starts getting up process.
            case KeyCode.Control =>{
                                     controlPressed     = false
                                     gettingUp          = true
                                     leaningUp          = true
            }
            // Similarly releasing shift starts recoveral.
            case KeyCode.Shift   =>{
                                     shiftPressed       = false
                                     if (!endlessStamina) {
                                       recovering         = true
                                     }
                                     sprinting          = false
            }
            case KeyCode.Plus    =>  zoomingIn        = false
            case KeyCode.Minus   =>  zoomingOut       = false
            case _               =>  
          }
        }
        
        onMouseMoved = (event: MouseEvent) => {
            mouseDeltaX = event.screenX - centerPoint._1
            mouseDeltaY = event.screenY - centerPoint._2
            Camera.rotate(mouseDeltaY * sensitivity / (2*Pi), -mouseDeltaX * sensitivity  / (2*Pi) , 0)
            robot.mouseMove(centerPoint._1.toInt, centerPoint._2.toInt)
        }
        onMouseDragged = (event: MouseEvent) => {
            mouseDeltaX = event.screenX - centerPoint._1
            mouseDeltaY = event.screenY - centerPoint._2
            Camera.rotate(mouseDeltaY * sensitivity / (2*Pi), -mouseDeltaX * sensitivity  / (2*Pi) , 0)
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

