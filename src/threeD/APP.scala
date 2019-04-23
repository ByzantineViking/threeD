package threeD

import scalafx.Includes._

import scalafx.application.JFXApp


import scalafx.scene.input._
import scalafx.scene.canvas._
import scalafx.scene.layout.VBox
//paths and fill
import scalafx.scene.{Group, Node}
import scalafx.scene.{shape => S}

import scalafx.scene.{control => C}
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.text.Font

//Basic
import scala.collection.mutable.Buffer
import scalafx.scene.paint.Paint
import java.awt.{GraphicsEnvironment, GraphicsDevice}

import scalafx.application.AppHelper

object BootStrap extends JFXApp {
  val header = Font.font("Gill Sans MT", 20)
  val subHeader = Font.font("Gill Sans MT", 15)
  val text = Font.font("Gill Sans MT", 12)
  val defaultColor = Color.AliceBlue
  
  stage = new JFXApp.PrimaryStage {
        val screenSize:  (Double, Double) = (GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice.getDefaultConfiguration.getBounds.getWidth,
                                         GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice.getDefaultConfiguration.getBounds.getHeight)
       var root = new Group()
       
        val cont = new Group()
        
        val rectLeft = S.Rectangle(60,screenSize._2)
        rectLeft.fill = Color.AliceBlue
        rectLeft.stroke_=(Color.Black)
        rectLeft.layoutX = 0
        
        
        val rectRight = S.Rectangle(60,screenSize._2)
        rectRight.fill = Color.AliceBlue
        rectRight.stroke_=(Color.Black)
        rectRight.layoutX = screenSize._1 - 60
        
        
        val label1 = C.Label("threeD")
        label1.layoutX = 390
        label1.layoutY = 40
        label1.font = header
        
        
        val label2 = C.Label("Settings")
        label2.layoutX = 200
        label2.layoutY = 60
        label2.font = subHeader
      
        cont.getChildren.addAll(rectLeft, rectRight, label1, label2)
        
        
        val options: Scene = new Scene(cont) {
            onKeyPressed = (event: KeyEvent) => {
              event.code match {
                case KeyCode.Escape  =>  scene = bootstrap
                case KeyCode.Enter   =>  
                case _ =>
              }
            }
        }
       
       val startView = new Group()
       startView.getChildren.addAll(rectLeft, rectRight)
       
       val bootstrap: Scene = new Scene(startView) {
            onKeyPressed = (event: KeyEvent) => {
              event.code match {
                case KeyCode.Escape  =>  System.exit(1)
                case KeyCode.Enter   =>  scene = options
                case _ =>
              }
            }
        }
       
       
       scene = bootstrap
              
  }
}