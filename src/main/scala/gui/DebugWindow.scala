package gui

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


import gameengine.Engine

import scala.collection.mutable


/**
 * DebugWindow is a tool with debugging info to display while developing the software.
 *
 * As it is an object, you must name it somewhere in order to use it.
 */
object DebugWindow extends Frame("DebugWindow", Some(UIParent)) {
  setSize(300)
  setPoint(Center)
  setFrameStrata(TooltipStratum)


  private object Header extends Frame("", Some(this)) {
    setPoint(Top)
    setSize(300, 30)

    private val tex = createTexture()
    tex.setAllPoints()
    tex.setVertexColor(0,0,0)

    private val fs = createFontString()
    fs.setAllPoints()
    fs.setText("Debug Window")
  }

  registerForDrag(Header)


  private val bg = createTexture()
  bg.setPoint(Bottom)
  bg.setSize(300, 270)
  bg.setVertexColor(1,0,0,0.5)


  private val scrollFrame = new ScrollFrame(this)
  scrollFrame.setPoint(BottomLeft)
  scrollFrame.setSize(270)

  private object ScrollChild extends Frame(this) {
    setSize(scrollFrame.width - 1, 15)
    private val topFS = createFontString()
    topFS.setSize(scrollFrame.width - 10, 30)
    topFS.setFontSize(20)
    topFS.setPoint(Top, this, Top, 10)
    topFS.setJustifyH(JustifyLeft)
    topFS.setText("Data:")
    topFS.setVertexColor(0,0,0)

    private var data: List[(FontString, (Double) => String)] = List((topFS, (_) => {"Data:"}))

    def addData(dataOrigin: (Double) => String): Unit = {
      val fs = createFontString()
      fs.setSize(scrollFrame.width - 10, 20)
      fs.setFontSize(15)
      fs.setPoint(Top, data.head._1, Bottom)
      fs.setJustifyH(JustifyLeft)
      fs.setText(dataOrigin.apply(0))
      fs.setVertexColor(0,0,0)

      data = (fs, dataOrigin) +: data
      this.setHeight(this.height + 20)
      scrollFrame.updateScrollChildRect()
    }

    setScript(ScriptKind.OnUpdate)((_: Frame, dt: Double) =>
      data.foreach({case (fs, dataOrigin) => fs.setText(dataOrigin.apply(dt))})
    )
  }

  private object ScrollBar extends Slider(this) {
    private val outer = DebugWindow.this
    setPoint(TopRight, outer, TopRight, 0, -30)
    setPoint(BottomRight)
    setWidth(15)

    thumbTexture.setWidth(15)
    setStep(Some(1))
    setOrientation(VerticalBar)

    var speed: Double = 0
    private val acceleration: Double = 5

    setScript(ScriptKind.OnUpdate)((_: Frame, dt: Double) => {
      if (speed != 0) {
        setValue(value + speed)
        val (min, max) = minMaxValues
        if (value == min || value == max)
          speed = 0
        if (speed > 0)
          speed = math.max(0, speed - acceleration * dt / 1000)
        else if (speed < 0)
          speed = math.min(0, speed + acceleration * dt / 1000)
      }
    })

    private val bg = createTexture()
    bg.setAllPoints()
    bg.setVertexColor(0.8,0.8,0.8)

    setScript(ScriptKind.OnValueChanged)((_: ValueBar, value: Double, _: Double) => {
      scrollFrame.setVerticalScroll(scrollFrame.verticalScrollRange - value)
    })

    setScript(ScriptKind.OnMinMaxValuesChanged)((_: ValueBar, min: Double, max: Double) => {
      setThumbLength(math.max(20, height - max + min))
    })

    setMinMaxValues(0,0)
  }

  ScrollBar

  setScript(ScriptKind.OnWheelMoved)((self: Frame, _: Int, dy: Int) => {
    val (x, y) = Engine.mousePosition
    if (self.isMouseOver(x, y)) {
      if (dy > 0) {
        if (ScrollBar.speed > 0)
          ScrollBar.speed = 0
        ScrollBar.speed = ScrollBar.speed - 5
      } else if (dy < 0) {
        if (ScrollBar.speed < 0)
          ScrollBar.speed = 0
        ScrollBar.speed = ScrollBar.speed + 5
      }
    }
  })

  scrollFrame.setScript(ScriptKind.OnScrollRangeChanged)((_: ScrollFrame, _: Double, range: Double) => {
    ScrollBar.setMinMaxValues(0, range)
    ScrollBar.setValue(ScrollBar.value)
  })

  scrollFrame.setScrollChild(ScrollChild)
  scrollFrame.updateScrollChildRect()

  ScrollChild.addData((_) => {
    val (x, y) = Engine.mousePosition
    "Mouse position: " + x.toInt + ", " + y.toInt
  })

  private val fpsRecords = mutable.Queue[Int]()
  for (_ <- 0 until 60) fpsRecords.enqueue(60)

  ScrollChild.addData((dt: Double) => {
    fpsRecords.enqueue((1000 / dt).toInt)
    while (fpsRecords.length > 200) fpsRecords.dequeue()
    "FPS: " + (fpsRecords.sum / fpsRecords.size + 1)
  })

  ScrollChild.addData((_) => {
    "Object nbr: " + UIObject.uiObjectNumber
  })

  ScrollChild.addData((_) => {
    if (Frame.topMouseFrame != null) "Top frame: " + Frame.topMouseFrame.toString else "Top frame:"
  })


  /**
   * Adds a line of data to display in the DebugWindow.
   *
   * @param dataOrigin function that returns a value to display. The argument given to it is the dt
   *                   of the OnUpdate handler.
   */
  def addData(dataOrigin: (Double) => String): Unit = ScrollChild.addData(dataOrigin)
}
