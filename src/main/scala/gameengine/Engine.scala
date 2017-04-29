/**
 * License
 * =======
 *
 * The MIT License (MIT)
 *
 *
 * Copyright (c) 2017 Antoine DOERAENE @sherpal
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/


package gameengine

import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.raw.{CanvasRenderingContext2D, Event, HTMLImageElement}
import webglgraphics.Painter

import scala.scalajs.js.timers._
import scala.collection.mutable


object Engine {

  // this is the canvas the game draws onto
  // the id of the Html canvas element must be "canvas"
  private val canvas: html.Canvas = dom.document.getElementById("canvas").asInstanceOf[html.Canvas]
  canvas.height = dom.window.innerHeight.toInt - 10
  canvas.width = dom.window.innerWidth.toInt - 10

  //private val gl: WebGLRenderingContext = canvas.getContext("webgl").asInstanceOf[WebGLRenderingContext]
  private val ctx: CanvasRenderingContext2D = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]


  // Painter used to draw on the Canvas
  //val painter: Painter = new Painter(canvas, gl)
  val painter: Painter = new Painter(canvas, ctx)

  // this is the game state currently running, for example the menus, the game itself, loading screens...
  private var _gameState: GameState = new GameState()

  // these two variables keep track of whether the main loop should stop and whether it is running
  private var _stopGameLoop: Boolean = false
  private var _gameLoopRunning: Boolean = false



  /// Key presses callbacks ///

  // this set contains all the keys that are down
  private val downKeys: mutable.Set[String] = mutable.Set()

  def isDown(key: String): Boolean = downKeys contains key

  canvas.addEventListener[dom.KeyboardEvent]("keydown", (event: dom.KeyboardEvent) => {
    // calling keyPressed before adding the key to the downKeys set to check if key is repeated
    _gameState.keyPressed(event.key, isDown(event.key))
    downKeys += event.key
  })

  canvas.addEventListener[dom.KeyboardEvent]("keyup", (event: dom.KeyboardEvent) => {
    _gameState.keyReleased(event.key)
    downKeys -= event.key
  })



  /// Mouse Event

  private val downMouse: mutable.Set[Int] = mutable.Set()

  private def effectiveMousePos(clientX: Double, clientY: Double): (Double, Double) = {
    val boundingRect = canvas.getBoundingClientRect()
    (clientX - boundingRect.left - canvas.width / 2, -clientY + boundingRect.top + canvas.height / 2)
  }

  canvas.onmousedown = (event: dom.MouseEvent) => {
    val (x, y) = effectiveMousePos(event.clientX, event.clientY)
    _gameState.mousePressed(x, y, event.button)
    downMouse += event.button
  }

  canvas.onmouseup = (event: dom.MouseEvent) => {
    val (x, y) = effectiveMousePos(event.clientX, event.clientY)
    _gameState.mouseReleased(x, y, event.button)
    downMouse -= event.button
  }

  private var lastMousePosition: (Double, Double) = (0, 0)

  def mousePosition: (Double, Double) = lastMousePosition

  canvas.onmousemove = (event: dom.MouseEvent) => {
    val (x, y) = effectiveMousePos(event.clientX, event.clientY)
    _gameState.mouseMoved(
      x, y, x - lastMousePosition._1, y - lastMousePosition._2, event.button
    )
    lastMousePosition = (x, y)
  }

  canvas.onmousewheel = (event: dom.WheelEvent) => {
    // deltaY is positive to the bottom, negative to the top
    _gameState.mouseWheel(event.deltaX.toInt, event.deltaY.toInt, event.deltaZ.toInt)
  }

  /// focus lost and received callbacks

  canvas.addEventListener[dom.FocusEvent]("focusout", (_: dom.FocusEvent) => {
    downKeys.clear()
    downMouse.clear()
  })

  canvas.addEventListener[dom.FocusEvent]("focusin", (_: dom.FocusEvent) => {
    // things to do?
  })

  private var _computationTime: Double = 0
  def computationTime: Double = _computationTime

  private val defaultRun: () => Unit = () => {
    _gameState.load()

    var lastTimeStamp = 0.0
    def mainLoop(timeStamp: Double): Unit = {
      val t = new java.util.Date().getTime
      val dt = timeStamp - lastTimeStamp
      lastTimeStamp = timeStamp

      // TODO: manage events

      _gameState.update(dt)

      painter.resetTransformationMatrix()
      painter.clear()

      _gameState.draw()



      if (_stopGameLoop) resetGameLoop()
      else dom.window.requestAnimationFrame((ts: Double) => mainLoop(ts))
      _computationTime = new java.util.Date().getTime - t
    }

    mainLoop(0)
  }

  var run: () => Unit = () => {}


  def changeGameState(state: GameState): Unit = {
    _gameState = state
    _stopGameLoop = true
    run = _gameState.run match {
      case None => defaultRun
      case Some(f) => f
    }
  }
  changeGameState(_gameState) // setting a default run function

  def gameState: GameState = _gameState

  def loadAssets(assets: List[HTMLImageElement]): Unit = {
    var loaded: Int = 0
    assets.foreach(a => a.onload = (_: Event) => loaded += 1)

    def loop(): Unit = {
      if (loaded == assets.length) assetsLoaded(assets)
      else setTimeout(500) {
        loop()
      }
    }

    loop()
  }

  def assetsLoaded(assets: List[HTMLImageElement]): Unit = {}


  private def resetGameLoop(): Unit = {
    _stopGameLoop = false
    _gameLoopRunning = false
  }


  def startGameLoop(): Unit = {
    _stopGameLoop = false
    _gameLoopRunning = true

    run()
  }

  def stopGameLoop(): Unit = {
    _stopGameLoop = true
  }

}
