package threeD

import scalafx.Includes._

import scalafx.application.JFXApp


import scalafx.scene.input._
import scalafx.scene.canvas._
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

import scalafx.application.AppHelper

object BootStrap extends JFXApp {
  val header = Font.font("Gill Sans MT", 20)
  val subHeader = Font.font("Gill Sans MT", 15)
  val text = Font.font("Gill Sans MT", 12)
  val defaultColor = Color.AliceBlue
  
  stage = new JFXApp.PrimaryStage {
      scene = new Scene(800,400) {
        val cont = Buffer[Node]()
        
        
        val rectLeft = S.Rectangle(60,400)
        rectLeft.fill = Color.AliceBlue
        rectLeft.stroke_=(Color.Black)
        rectLeft.layoutX = 0
        cont += rectLeft
        
        val rectRight = S.Rectangle(60,400)
        rectRight.fill = Color.AliceBlue
        rectRight.stroke_=(Color.Black)
        rectRight.layoutX = 740
        cont += rectRight
        
        
        val label1 = C.Label("threeD")
        label1.layoutX = 390
        label1.layoutY = 40
        label1.font = header
        cont += label1
        
        
        val label2 = C.Label("Settings")
        label2.layoutX = 200
        label2.layoutY = 60
        label2.font = subHeader
        cont += label2
      
      onKeyPressed = (event: KeyEvent) => {
        event.code match {
          case KeyCode.Escape  =>  System.exit(1)
          case KeyCode.Enter   =>  {
            System.exit(1)
            Front.main(Array())
            
          }
          case _ =>
        }
      }
      
      content = cont
    }
  }
}