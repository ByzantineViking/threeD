package threeD

// App
import scalafx.Includes._

import scalafx.application.JFXApp

import scalafx.scene.input.{KeyCode, KeyCombination, KeyCodeCombination, KeyEvent, MouseEvent}
import scalafx.event.ActionEvent
//import scalafx.event.ActionEvent
import scalafx.beans.value.ObservableValue
// To keep the mouse in the middle
import java.awt.Robot
import java.awt.{GraphicsEnvironment, GraphicsDevice}

import scalafx.scene.canvas._
//paths and fill
import scalafx.scene.{Group, SceneAntialiasing, Node}
import scalafx.scene.shape._

import scalafx.scene.Scene
import scalafx.scene.paint.Color
import javafx.scene.Cursor
import scalafx.scene.text._
import scalafx.scene.text.Font
import scalafx.scene.layout._
import scalafx.geometry.{Insets}
import scalafx.scene.image.{Image, ImageView}


import scalafx.scene.{shape => S}

import scalafx.scene.{control => C}
import scalafx.scene.effect._

//Basic
import scala.collection.mutable.Buffer
import scala.collection.mutable.Queue

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
        var sensitivity  : Double       = 3 * pow(10, -3)
        var stamina      : Double       = 5
        // Recoveral time in seconds.
        var recoveralTime: Double       = 1.0
        var doubleJump   : Boolean      = false
        var endlessStamina: Boolean     = false
        var flying       : Boolean      = false
        
//--------------------------------------------------------------------------------//
        
         // Clips the triangle behind camera, but decreases performance
        var clippingEnabled             = false
        
//--------------------------------------------------------------------------------//
  
        // Setting the size of the scene
        var base = 250
        var widthAspect = 4
        var heightAspect = 3
        
        
        
//--------------------------------------------------------------------------------//
        // For loading a CSV file
        var fileName = "data"
        var fileEnding = ".csv"
        
        // If false, read object, if true, read CSV
        var fileIsCSV = true
//--------------------------------------------------------------------------------//
// END OF SETTINGS. 
        
    // Setting up
    Writer
    
    var fullData = Projector.project
    var data = fullData._1
    var distances: Array[Double] = fullData._2.map(triangle => MathHelper.triangleDistanceFromCameraPoint(triangle))
    
    
    
    
        
        
        
    // Robot moves the mouse back to the center of the screen. If the processor processes all the frames, the mouse stays within the window.
    val robot = new Robot()
    // Center of the whole screen, independent of monitor size.
    val centerPoint: (Int, Int) = (GraphicsEnvironment.getLocalGraphicsEnvironment().getCenterPoint.getX.toInt, GraphicsEnvironment.getLocalGraphicsEnvironment().getCenterPoint.getY.toInt)
    val screenSize:  (Double, Double) = (GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice.getDefaultConfiguration.getBounds.getWidth,
                                         GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice.getDefaultConfiguration.getBounds.getHeight)
    
    // Fonts
    val header = Font.font("Segoe UI", 32)
    val subHeader = Font.font("Segoe UI", 18)
    val defaultColor = Color.Black
    
    
    var staminaLeft = stamina
    var recoveralTimeLeft = recoveralTime
    
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
    
    private var screenHeightHolder: Double = base * heightAspect
    private var screenWidthHolder: Double = base * widthAspect
    
//-------------------------------------------------------------------------------------------//
    private var goingFull = false
    
    private var dragging = false
    
    
    
    
    
    stage = new JFXApp.PrimaryStage {
        // Creating the size of the stage. Scene is set to same size.
        private[threeD] def handleFullScreen = {
          if (goingFull) {
            this.fullScreen_= (goingFull)
            this.fullScreenExitHint_=("Press \"F\" to exit full screen.\nESC to quit")
            this.fullScreenExitKey_=(KeyCombination("f"))
          }
        }  
        
//-----------------------------------------------------------------------------------------------------------------------------//
       
       val startRoot = new Group()
       val bootstrap: Scene = new Scene(screenWidthHolder, screenHeightHolder) {
           
            
            val ds = new DropShadow(200, Color.Black)
            ds.setOffsetX(5)
            ds.setOffsetY(5)
            ds.setColor(Color.Black)
         
           
            val startButton = new C.Button("Start")
            startButton.font = subHeader
            
            val optionsButton = new C.Button("Options")
            optionsButton.font = subHeader

            val quitButton = new C.Button("Quit")
            quitButton.font = subHeader

             val label1 = new C.Label("threeD")
            label1.font = header
            label1.effect = ds
            
            val box = new VBox(screenHeightHolder/ 6 )
            box.padding = Insets(screenHeightHolder /6.0,0, screenHeightHolder, screenWidthHolder/2.2)
            box.children = List(label1, startButton, optionsButton, quitButton)
            
            
            // Setting the content
            root = box
            
            
            width.onChange((observable, oldvalue, newvalue) => {
              box.padding = Insets(box.padding.value.top, 0, 0, newvalue.doubleValue()/2.2)
              screenWidthHolder = newvalue.doubleValue()
            })
            height.onChange((observable, oldvalue, newvalue) => {
              box.padding = Insets(newvalue.doubleValue()/6.0, 0, 0, box.padding.value.left)
              box.spacing = box.spacing.value * (newvalue.doubleValue()/oldvalue.doubleValue())
              screenHeightHolder = newvalue.doubleValue()
            })
            
            stylesheets += "stylesheets/bootstrap.css"
            
            
            
            // Event handlers 
            startButton.onAction = (e: ActionEvent) => {
              goingFull = !goingFull
              scene = threeD
            }
            optionsButton.onAction = (e: ActionEvent) => {
              scene = options
            }
            quitButton.onAction = (e: ActionEvent) => {
              System.exit(1)
            }
            onKeyPressed = (event: KeyEvent) => {
              event.code match {
                case KeyCode.Enter  =>  
                case KeyCode.Escape  =>  System.exit(1)
                case KeyCode.F       => goingFull        = !goingFull
                case _ =>
              }
            }
        }
        
//-----------------------------------------------------------------------------------------------------------------------------//

        val optionsRoot = new Group()
        val options: Scene = new Scene(screenWidthHolder, screenHeightHolder) {
            // Content for the bootstrap and options screens.
            val ds = new DropShadow(200, Color.Black)
            ds.setOffsetX(5)
            ds.setOffsetY(5)
            ds.setColor(Color.Black)
                        
            
            // Headers
            val header1 = new C.Label {
              text = "Options"
              font = header
            }

//--------------------------------------------------------//
            
            
            
            
            
            
//--------------------------------------------------------//
            // Stats
            val header2 = new C.Label {
              text = "Stats"
              font = header
            }
            val label4 = new C.Label {
              text = "Speed"
              font = subHeader
              effect = ds
            }
            val slider1 = new C.Slider {
              min = 1.0
              max = 5.0
              value = 3.0
              showTickMarks = true
              showTickLabels = true
              blockIncrement = 0.1
              majorTickUnit = 5.0
            }
            val label5 = new C.Label {
              text = "Time to recover"
              font = subHeader
              effect = ds
            }
            val slider2 = new C.Slider {
              min = 1.0
              max = 5.0
              value = 3.0
              showTickMarks = true
              showTickLabels = true
              blockIncrement = 0.1
              majorTickUnit = 5.0
            }
            val label6 = new C.Label {
              text = "Sensitivity"
              font = subHeader
              effect = ds
            }
            val slider3 = new C.Slider {
              min = 1.0
              max = 5.0
              value = 3.0
              showTickMarks = true
              showTickLabels = true
              blockIncrement = 0.1
              majorTickUnit = 5.0
            }
            val label7 = new C.Label {
              text = "Stamina"
              font = subHeader
              effect = ds
            }
            val slider4 = new C.Slider {
              min = 1.0
              max = 5.0
              value = 3.0
              showTickMarks = true
              showTickLabels = true
              blockIncrement = 0.1
              majorTickUnit = 5.0
            }
//--------------------------------------------------------//
            
            
            // Cheats
            val header4 = new C.Label {
              text = "Cheats"
              font = header
            }
            val label1 = new C.Label {
              text = "Double jump"
              font = subHeader
              effect = ds
            }
            val check1 = new C.CheckBox {
              this.selected.onChange(doubleJump = !doubleJump)
              effect = ds
            }
            val label2 = new C.Label {
              text = "Flying"
              font = subHeader
              effect = ds
            }
            val check2 = new C.CheckBox {
              this.selected.onChange(flying = !flying)
              effect = ds
            }
            val label3 = new C.Label {
              text = "Endless Stamina"
              font = subHeader
              effect = ds
            }
            val check3 = new C.CheckBox {
              this.selected.onChange(endlessStamina = !endlessStamina)
              effect = ds
            }
//--------------------------------------------------------//
            val button1 = new C.Button {
              text = "View more"
              font = subHeader
            }
            val button2 = new C.Button {
              text = "Back"
              font = subHeader
            }
            val button3 = new C.Button {
              text = "Controls"
              font = subHeader
            }
            
            
            
            
            val grid = new GridPane()
            grid.add(header1, 0, 0)
            
            // Stats
            grid.add(header2, 0, 1)
            grid.add(label4, 0, 2)
            grid.add(slider1,0, 3)
            grid.add(label5, 1, 2)
            grid.add(slider2,1, 3)
            grid.add(label6, 2, 2)
            grid.add(slider3,2, 3)
            grid.add(label7, 3, 2)
            grid.add(slider4,3, 3)
            
            // Cheats
            grid.add(header4, 0, 4)
            grid.add(label1, 0, 5)
            grid.add(check1, 0, 6)
            grid.add(label2, 1, 5)
            grid.add(check2, 1, 6)
            grid.add(label3, 2, 5)
            grid.add(check3, 2, 6)
            
            
            
            val subGrid = new GridPane()
            subGrid.add(button1, 0, 0)
            subGrid.add(button2, 1, 0)
            subGrid.hgap = 10
            grid.add(subGrid, 2, 0)
            grid.add(button3, 3, 0)
            
            
            
//--------------------------------------------------------//  
            
            
            grid.padding = Insets(screenHeightHolder /8.0,screenWidthHolder/8.0, 0, screenWidthHolder/8.0)
            grid.vgap = screenHeightHolder /12.0
            grid.hgap = screenWidthHolder /12.0
            
//            optionsRoot.getChildren.addAll(label1)
            content = grid
            
            stylesheets += "stylesheets/options.css"

            // Event handlers 
            width.onChange((observable, oldvalue, newvalue) => {
              grid.padding = Insets(grid.padding.value.top, newvalue.doubleValue()/8.0, 0, newvalue.doubleValue()/8.0)
              grid.hgap = newvalue.doubleValue() / 12.0
              screenWidthHolder = newvalue.doubleValue()
            })
            height.onChange((observable, oldvalue, newvalue) => {
              grid.padding = Insets(newvalue.doubleValue()/8.0, 0, 0, grid.padding.value.left)
              grid.vgap = newvalue.doubleValue() / 12.0
              screenHeightHolder = newvalue.doubleValue()

            })
            
            onKeyPressed = (event: KeyEvent) => {
              event.code match {
                case KeyCode.Escape  =>  scene = bootstrap
                case KeyCode.F       =>  goingFull        = !goingFull
                case _ =>
              }
            }
            button1.onAction = (e: ActionEvent) => {
              scene = more
            }
            button2.onAction = (e: ActionEvent) => {
              scene = bootstrap
            }
            button3.onAction = (e: ActionEvent) => {
              scene = keyboardScene
            }
            
            slider1.valueProperty().onChange((observable, oldvalue, newvalue) => {
              playerSpeed = (newvalue.doubleValue()/3.0)
            })
            slider2.valueProperty().onChange((observable, oldvalue, newvalue) => {
              recoveralTime      = (newvalue.doubleValue()/3.0) * 2.0
              recoveralTimeLeft  = (newvalue.doubleValue()/3.0) * 2.0
            })
            slider3.valueProperty().onChange((observable, oldvalue, newvalue) => {
              sensitivity = (newvalue.doubleValue()/3.0)* 2.0 * pow(10, -3)
            })
            slider4.valueProperty().onChange((observable, oldvalue, newvalue) => {
              stamina = (newvalue.doubleValue()/3.0)*5.0
              staminaLeft = (newvalue.doubleValue()/3.0)*5.0
            })
            
        }
//-----------------------------------------------------------------------------------------------------------------------------//
        
        
        val keyboardScene: Scene = new Scene(screenWidthHolder, screenHeightHolder) {
            
            val keyView = new ImageView(new Image("resources/keyboardLayout.png"))
            keyView.preserveRatio = true
            keyView.fitWidth = screenWidthHolder
            
            
            val button1 = new C.Button {
              text = "Back"
              font = subHeader
              layoutX = screenWidthHolder - screenWidthHolder/40.0

            }
            
            
            
            
            val grid = new GridPane()
            grid.add(button1, 0, 0)
            grid.add(keyView, 0, 1)
            grid.layoutY = screenHeightHolder /40.0
            grid.vgap = screenHeightHolder /40.0
            content = grid
            
            stylesheets += "stylesheets/options.css"

//--------------------------------------------------------//
            
            // Event handlers 
            width.onChange((observable, oldvalue, newvalue) => {
              keyView.fitWidth = newvalue.doubleValue()
              screenWidthHolder = newvalue.doubleValue()
            })
            height.onChange((observable, oldvalue, newvalue) => {
              grid.vgap = newvalue.doubleValue() / 40.0
              screenHeightHolder = newvalue.doubleValue()
            })
            
            
            onKeyPressed = (event: KeyEvent) => {
                event.code match {
                  case KeyCode.Escape  =>  scene = options
                  case KeyCode.F       =>  {
                    goingFull        = !goingFull
                  }
                  case _ =>
                }
              }
              button1.onAction = (e: ActionEvent) => {
                scene = options
              }
        }
        
//-----------------------------------------------------------------------------------------------------------------------------//
        val moreRoot = new Group()
        val more: Scene = new Scene(screenWidthHolder, screenHeightHolder) {
            var currentWidth = this.width.doubleValue()
            var currentHeight = this.height.doubleValue()
            // Content for the bootstrap and options screens.
            val ds = new DropShadow(200, Color.Black)
            ds.setOffsetX(5)
            ds.setOffsetY(5)
            ds.setColor(Color.Black)
            
            
            
            // Headers
            val header1 = new C.Label {
              text = "Options"
              font = header
            }
            
//--------------------------------------------------------//
            // Aspect Ratio
            val header5 = new C.Label {
              text = "Aspect Ratio"
              font = subHeader
              effect = ds
            }
            
            val menu1 = new C.ComboBox(Seq("4:3","7:5","16:9","21:9", "9:16")) {
              value = "4:3"
              this.onAction = (e: ActionEvent) => {
                widthAspect = value.value.takeWhile(_!= ':').toInt
                heightAspect = value.value.dropWhile(_!= ':').drop(1).toInt
                value = value.value
                if (widthAspect * base > screenSize._1){
                  val ratio = screenSize._1 / (widthAspect * base)
                  screenHeightHolder = heightAspect * base * ratio
                  screenWidthHolder = screenSize._1
                  if (screenHeightHolder > screenSize._2) {
                    val ratio = screenSize._2 / screenHeightHolder
                    screenWidthHolder = screenWidthHolder * ratio
                    screenHeightHolder = screenSize._2
                   
                  }
                } else {
                  if (base * heightAspect > screenSize._2) {
                      val ratio = screenSize._2 / (base * heightAspect)
                      screenWidthHolder = base * widthAspect * ratio
                      screenHeightHolder= screenSize._2
                  } else {
                    screenHeightHolder = heightAspect * base
                    screenWidthHolder = widthAspect * base
                  }
                }
              }
             
            }
//--------------------------------------------------------//
            // Size
            val header7 = new C.Label {
              text = "Size"
              font = subHeader
              effect = ds
            }
            
            val menu2 = new C.ComboBox(Seq("puny","small","medium","big", "enormous")) {
              value = "medium"
              this.onAction = (e: ActionEvent) => {
                  value = value.value
                  value.value match {
                    case "puny"     => base = 50
                    case "small"    => base = 175
                    case "medium"   => base = 250
                    case "big"      => base = 325
                    case "enormous" => base = 400
                  }
              }
            }
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
//--------------------------------------------------------//
            
            // File loading
            val header6 = new C.Label {
              text = "File Name"
              font = subHeader
            }
            val textField = new C.TextField {
              promptText = "e.g. house"
              this.focused.onChange({
                  text = text.value
              })
            }
            val ending = new C.ComboBox(Seq(".csv", ".obj")) {
              value = ".csv"
              this.onAction = (e: ActionEvent) => {
                value = value.value
              }
            }
            val button2 = new C.Button {
              text = "Load"
            }
            val sub = new GridPane
            sub.add(textField, 0, 0)
            sub.add(ending, 1, 0)
            sub.add(button2,2,0)
            sub.vgap = 3
            sub.hgap = 3
//--------------------------------------------------------//
            
            // Performance
            val header3 = new C.Label {
              text = "Extra clipping"
              font = subHeader
            }
            
            // Replacing tickbox with a image needs more tuning.
            
//            val uncheckedPath : String = CSVReader.fileHelper("unchecked.png")
//            val help1: Image = new Image("file:" + uncheckedPath) 
//            val viewI = new ImageView(help1)
//            viewI.layoutX = 0
//            val checkedPath : String = CSVReader.fileHelper("checked.png")
//            val help2: Image = new Image("file:" + checkedPath) 
//            val viewII = new ImageView(help2)
            
            val perf = new C.CheckBox {
              effect = ds
              this.selected = false
              this.selected.onChange(clippingEnabled = !clippingEnabled)
            }
//--------------------------------------------------------// 
            val button1 = new C.Button {
              text = "Back"
              font = subHeader
            }
 
            
            
                 
            
            
            
//--------------------------------------------------------//       
            val grid = new GridPane()
            grid.add(header1, 0, 0)
            
            
            
            // Performance
            grid.add(header3, 0, 1)
            grid.add(perf, 0, 2)
            
            // Aspect Ratio
            grid.add(header5, 1, 1)
            grid.add(menu1, 1, 2)
            
            // Size
            grid.add(header7, 1, 4)
            grid.add(menu2, 1, 5)
            
            
            // File loading
            grid.add(header6, 2, 1)
            grid.add(sub, 2, 2)
            
            
            grid.add(button1, 2, 0)
//--------------------------------------------------------// 
            
            
            grid.padding = Insets(screenHeightHolder /8.0,screenWidthHolder/8.0, 0, screenWidthHolder/8.0)
            grid.vgap = screenHeightHolder /12.0
            grid.hgap = screenWidthHolder /12.0
            
//            optionsRoot.getChildren.addAll(label1)
            content = grid
            
            stylesheets += "stylesheets/options.css"
            
            
            
            
//--------------------------------------------------------// 
            
            
            // Event handlers 
            width.onChange((observable, oldvalue, newvalue) => {
              grid.padding = Insets(grid.padding.value.top, 0, 0, newvalue.doubleValue()/oldvalue.doubleValue()*grid.padding.value.left)
              grid.hgap = newvalue.doubleValue() / 12.0
            })
            height.onChange((observable, oldvalue, newvalue) => {
              grid.padding = Insets(newvalue.doubleValue()/oldvalue.doubleValue()*grid.padding.value.top, 0, 0, grid.padding.value.left)
              grid.vgap = newvalue.doubleValue() / 12.0

            })
            
            
            onKeyPressed = (event: KeyEvent) => {
              event.code match {
                case KeyCode.Escape  =>  scene = options
                case KeyCode.F       =>  goingFull        = !goingFull
                case _ =>
              }
            }
            button1.onAction = (e: ActionEvent) => {
              scene = options
            }
            
            
            val loaded = new C.Label("Loaded succesfully.")
            val notFound = new C.Label("Please enter file name.")
            button2.onAction = (e: ActionEvent) => {
              if (!textField.text.apply.isEmpty()) {
                    if (ending.value.apply() == ".obj") {
                      fileIsCSV = false
                      fileEnding = ".obj"
                      ending.value = ".obj"
                    } else {
                      fileIsCSV = true
                      fileEnding = ".csv"
                      ending.value = ".csv"
                    }
                    fileName = textField.text.apply.replaceAll("[^A-Za-z0-9()\\[\\]]", "")
                    if (sub.children.contains(notFound)) {
                      sub.children.removeAll(notFound)
                    }
                    if (sub.children.contains(loaded)) {
                      sub.children.removeAll(loaded)
                    }
                    sub.add(loaded, 4, 0)
                    
              } else {
                    if (sub.children.contains(loaded)) {
                      sub.children.removeAll(loaded)
                    }
                    if (sub.children.contains(notFound)) {
                      sub.children.removeAll(notFound)
                    }
                    sub.add(notFound, 4, 0)
              }
            }
            
            textField.onKeyPressed = (event: KeyEvent) => {
              event.code match {
                case KeyCode.Enter => {
                  val text = this.textField.text.apply
                  if (!text.isEmpty()) {
                    fileName = text.replaceAll("[^A-Za-z0-9()\\[\\]]", "")
                  } 
                }
                case _ => 
              }
            }
        }
        
//-----------------------------------------------------------------------------------------------------------------------------//
        
        
      // Main screen.
      val threeDRoot = new Group()
      val threeD: Scene = new Scene {
        cursor = Cursor.NONE
        //Drawing happens on the canvas, as it is quicker when dealing with having to re-draw everything every frame.
        val canvas = new Canvas(screenWidthHolder, screenHeightHolder)
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
        
        var recovering = false
        var replenishing = false
        
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
              
              // Handling fullscreen transitions
              if (goingFull) {
                screenWidthHolder = screenSize._1
                screenHeightHolder = screenSize._2
              } else {
                if (widthAspect * base > screenSize._1){
                  val ratio = screenSize._1 / (widthAspect * base)
                  screenHeightHolder = heightAspect * base * ratio
                  screenWidthHolder = screenSize._1
                  if (screenHeightHolder > screenSize._2) {
                    val ratio = screenSize._2 / screenHeightHolder
                    screenWidthHolder = screenWidthHolder * ratio
                    screenHeightHolder = screenSize._2
                  }
                } else {
                  if (base * heightAspect > screenSize._2) {
                      val ratio = screenSize._2 / (base * heightAspect)
                      screenWidthHolder = base * widthAspect * ratio
                      screenHeightHolder= screenSize._2
                      
                  } else {
                    screenHeightHolder = heightAspect * base
                    screenWidthHolder = widthAspect * base
                  }
                }
              }
              handleFullScreen
              
              canvas.width_=(screenWidthHolder)
              canvas.height=(screenHeightHolder)
              
              // Starts the canvas as a white background.
              gc.fill = Color.White
              gc.fillRect(0,0,screenWidthHolder,screenHeightHolder)
              
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
                  } else if (!jumping) {
                    recoveralTimeLeft -= delta
                    if (recoveralTimeLeft <= 0) {
                      replenishing = true
                      recoveralTimeLeft = recoveralTime
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
                var toBeAdded = 1.0
                if (x._1.abs > 100000) {
                  toBeAdded = 0.0 
                }
                if (x._2.abs > 100000) {
                  toBeAdded = 0.0 
                }
                    val croppedX = (scala.math.min(scala.math.max((x._1 + toBeAdded)/2.0 , -screenSize._1 /2.0),screenSize._1 /2.0) * screenWidthHolder).toInt
                    val croppedY = (scala.math.min(scala.math.max(((x._2 + toBeAdded)/2.0) , -10000.0),10000.0) * screenHeightHolder).toInt
//                    println((croppedX, croppedY))
                    (croppedX, croppedY)
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
              val relative = staminaLeft / stamina
              gc.fill = Color.rgb(61, 153, 122, .50)
              gc.fillRoundRect(screenWidthHolder/2.0 - (screenWidthHolder/2.0)/2.0, screenHeightHolder - screenHeightHolder/ 8.0, scala.math.min((screenWidthHolder/2.0) * relative, (screenWidthHolder/2.0)),30, 30, 30 )
              
              gc.stroke = Color.rgb(61, 153, 122, 1)
              gc.strokeRoundRect(screenWidthHolder/2.0  - (screenWidthHolder/2.0)/2.0 , screenHeightHolder - screenHeightHolder/ 8.0, screenWidthHolder/2.0,30, 30, 30 )
              
              if (dragging) {
                gc.fill = Color.rgb(61, 153, 122, 0.85)
                gc.fillRoundRect(screenWidthHolder /2.0 - (screenWidthHolder/3.7)/2.0, screenHeightHolder/ 10.0, screenWidthHolder/3.7, 60, 30, 30)
                gc.fill = Color.White
                gc.setFont(new Font("Segoe UI", 21))
                gc.fillText("Exit full-screen mode to move window around.", screenWidthHolder /2.0 - (screenWidthHolder/3.7)/2.0 + 30, screenHeightHolder/ 10.0 + 60/2.0 + 5)
                
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
            case KeyCode.F       =>  goingFull        = !goingFull
            case KeyCode.Space   =>  spacePressed     = true
            case KeyCode.Control =>  controlPressed   = true
            case KeyCode.Shift   =>  shiftPressed     = true
            case KeyCode.Plus    =>  zoomingIn        = true
            case KeyCode.Minus   =>  zoomingOut       = true
            case KeyCode.Z       =>  Camera.defaultZoom
            case KeyCode.Left    =>  {
              if (!goingFull) {
                if (stage.x.doubleValue() + stage.width.doubleValue() >= screenSize._1 - 20 ) {
                  stage.setX((screenSize._1 - stage.width.doubleValue()) / 2.0)
                } else {
                  stage.setX(-9)
                }
              }
            }
            case KeyCode.Right    =>  {
              if (!goingFull) {
                if (stage.x.doubleValue() < 20.0) {
                  stage.setX((screenSize._1 - stage.width.doubleValue()) / 2.0)
                } else {
                  stage.setX(screenSize._1 - stage.width.doubleValue() + 7)
                }
              }
            }
            case KeyCode.Up       =>  {
              if (!goingFull) {
                if (stage.y.doubleValue() >= 3.0) {
                  stage.setY(stage.y.doubleValue() - 5)
                } else {
                  stage.setY(-30)
                }
              }
            }
            case KeyCode.Down       =>  {
              if (!goingFull) {
                if (stage.y.doubleValue() <= -29.0) {
                  stage.setY(0)
                } else if (stage.y.doubleValue() + 10.0 <= screenSize._2 - stage.height.doubleValue()) {
                  stage.setY(stage.y.doubleValue() + 3)
                } else {
                  screenSize._2 - stage.height.doubleValue()
                }
              }
            }
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
            if (!goingFull) {
              stage.setX(event.screenX - screenWidthHolder/2.0 + (event.screenX - centerPoint._1) * 0.5)
              stage.setY(event.screenY - screenHeightHolder/2.0 + (event.screenY - centerPoint._2)* 0.5)
            } else {
              dragging = true
              
            }
        }
        onMouseReleased = (event: MouseEvent) => {
              dragging = false
        }
 //-------------------------------------------------------------------------------------------//
        
        //Scene settings.
        content = canvas
        title = "threeD"
      }  // End of scene
      scene = bootstrap
      
   }     // End of stage
        
}        // End of App
// As seen from the ending above, everything is done inside the scene, on the canvas, within the timer loop.

