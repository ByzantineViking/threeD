# threeD

#### Project Document 5/2019  

- The full project document can be found as [PDF](/src/Document.pdf)  

- Pictures at the end of README, as well as [here](/progress).  


## 1. Abstract  
The project is a from-scratch 3D engine running on Scala FX, written in Scala. Scala FX is
mostly a re-skin for Java FX and the two are both functional inside the app. I have taken
some functionality from Java FX, regarding the parts in which Scala FX documentation was
sparse. The engine is fully scalable with different 3D objects, as long as the data is
convertible to the format of three 3d points on a CSV row, each consisting of (x,z,y)-
coordinates forming a triangle in a clock-wise order. The project has a bug on
“Projection.scala”, on rows 145, and 146: A plane intersection edge case problem, where
the values scale too big and get drawn on the wrong corner of the screen.
## 2. User manual  
The program is started by launching “Front.scala” as a Scala Application. This opens up the
starting menu as a window, in which you can operate by using the mouse or keyboard.
Arrow keys allow movement between buttons, same as tab, and with space you can press
any button. Remember that if you are using arrow keys and enter a slider, it is not possible
to get out of it by left and right arrow keys, so use instead tab/up-down arrow keys. To
enter fullscreen mode you can press “F” any time, same to exit it. This is especially
preferable on the Keyboard shortcuts scene, to get a good view of the whole picture.
When you are done tinkering with settings, get back to the first scene, from which
you can choose “Start” to enter the main program. One final note about settings is that the
intersection math is rather heavy so if your hardware is having trouble, you can turn of
clipping under “Show more” in settings, but then you have to remember to always have the
objects in the world in front of you, as the clipping is integral to filtering out objects on the
backside of camera.
The controls are as follows. WASD to move around, mouse to turn around. Shift to
run, control to crouch. While crouched, Q and E to lean to the sides. Plus, to zoom in, minus
to zoom out. Z to reset zoom level to the original. To exit/enter full-screen, press F, and to
exit the program Esc. Space to jump around/ fly.
The program is best used as a visualization tool, so use of CSVWriter to make on
shapes is encouraged.

![Menu](/progress/bootstrap.png "Menu")
![Options](/progress/options_1.png "Options")
![KeyHelp](/progress/keyhelp.png "Controls")
![View1](/progress/grand_staircase_2.png "Stairs")
![View2](/progress/window.png "Window")
![View3](/progress/jumping_grounds.png "Blocks")
