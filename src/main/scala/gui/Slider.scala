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

package gui

import complex.Complex

/**
 * A Slider is used to set some property to a given value between min and max.
 * It can be used as a scroll bar for long [[ScrollFrame]], or as part as an option menu.
 */
class Slider(n: String = "", par: Option[Frame] = Some(UIParent)) extends Frame(n, par) with ValueBar {

  private var _thumbLength: Double = 20

  /** Returns the length the thumb should have. */
  def thumbLength: Double = _thumbLength

  /** Sets the length the thumb should have. */
  def setThumbLength(length: Double = 20): Unit = {
    _thumbLength = length

    updateTexture()
  }

  private var _enabled: Boolean = true

  /** Returns whether the Slider is enabled. */
  def isEnabled: Boolean = _enabled

  /** Enables the Slider to click specific events. */
  def enable(): Unit = {
    _enabled = true

    updateTexture()
  }

  /** Disables the Slider to click specific events. */
  def disable(): Unit = {
    _enabled = false

    updateTexture()
  }

  private var _step: Option[Double] = None

  /** Returns the step the value of the bar has to follow, or None if there isn't. */
  def step: Option[Double] = _step

  /** Sets the step value. */
  def setStep(s: Option[Double]): Unit = {
    _step = s

    updateTexture()
  }

  /** Returns the step the value of the bar has to follow, or 1 if there isn't. */
  def valueStep: Double = _step match {
    case Some(s) => s
    case None => 1
  }

  private var _thumbTexture: Texture = createTexture()
  _thumbTexture.setRadius(10)
  _thumbTexture.setVertexColor(0.5, 0.5, 0.5)
  def thumbTexture: Texture = _thumbTexture
  def setThumbTexture(): Unit = {
    _thumbTexture.removeParent()
    _thumbTexture = createTexture()
    _thumbTexture.setRadius(10)
    _thumbTexture.setVertexColor(0.5,0.5,0.5)

    updateTexture()
  }
  def setThumbTexture(file: String): Unit = {
    _thumbTexture.setVertexColor()
    _thumbTexture.setTexture(file)

    updateTexture()
  }
  def setThumbTexture(tex: Texture): Unit = {
    _thumbTexture.removeParent()

    if (scala.scalajs.LinkingInfo.developmentMode) {
      assert(tex.parent.isDefined && tex.parent.get == this, "parent of texture should be the slider")
    }

    _thumbTexture = tex

    updateTexture()
  }

  /**
   * Sets the value of the Slider.
   * The value will automatically be clamped between minimum and maximum values.
   * Fires the OnValueChanged script, even if the value set is the same as the previous value.
   */
  def setValue(v: Double): Unit = {
    val previousValue = findActualValue()

    val (minValue, maxValue) = minMaxValues
    _value = math.min(maxValue, math.max(minValue, v))

    val actualValue = updateValue()

    fires[ValueBar, Double, Double, Unit](ScriptKind.OnValueChanged)(actualValue, previousValue)
  }

  setPoint(AnchorPoint(Center, UIParent, Center))
  setSize(300, 30)
  setValue(0)

  clearAllPoints()
  setSize()

  /// Helper functions

  private def intervalLength(min: Double, max: Double, step: Double, length: Double): Double =
    length / math.ceil((max - min) / step)

  private def findActualValue(): Double = _step match {
    case Some(s) =>
      val (minValue, maxValue) = minMaxValues
      val intervalNbr = math.floor((_value - minValue) / s)
      val actualValue = minValue + intervalNbr * s
      math.min(maxValue, if (_value - minValue > (intervalNbr + 0.5) * s) actualValue + s else actualValue)
    case None => _value
  }

  private def findLeftAnchor(length: Double): Double = {
    val (minValue, maxValue) = minMaxValues
    _step match {
      case Some(s) =>
        val iL = intervalLength(minValue, maxValue, s, length)
        val intervalNbr = math.floor((_value - minValue) / s)
        if (value - minValue <= (intervalNbr + 0.5) * s) iL * intervalNbr else iL * (intervalNbr + 1)
      case None => (_value - minValue) / (maxValue - minValue) * length
    }
  }

  protected def updateValue(): Double = {
    updateTexture()

    findActualValue()
  }

  protected def updateTexture(): Unit = {
    val tex = _thumbTexture
    tex.clearAllPoints()

    orientation match {
      case HorizontalBar =>
        if (tex.isDisk) {
          tex.setRadius(height / 2)
        } else {
          tex.setHeight(height)
          tex.setWidth(thumbLength)
        }
        val leftAnchor = findLeftAnchor(width - tex.width)
        tex.setPoint(AnchorPoint(Center, this, Left, leftAnchor + tex.width / 2))
      case VerticalBar =>
        if (tex.isDisk) {
          tex.setRadius(width / 2)
        } else {
          tex.setWidth(width)
          tex.setHeight(thumbLength)
        }

        val bottomAnchor = findLeftAnchor(height - tex.height)
        tex.setPoint(AnchorPoint(Center, this, Bottom, 0, bottomAnchor + tex.height / 2))
    }
  }
}


object Slider {

  private var dragThumbInfo: Option[DragEventInfo] = None

  /**
   * Does everything that has to be done when a Slider is clicked.
   * This can be moving the thumb toward the mouse, or starting a drag procedure on the thumb.
   * Checking if the Slider is enabled is done before calling this method.
   */
  def onClickHandler(slider: Slider, x: Double, y: Double): Unit = {
    val thumb = slider._thumbTexture
    slider.orientation match {
      case HorizontalBar =>
        if (x < thumb.left) {
          slider.setValue(slider.value - slider.valueStep)
        } else if (x > thumb.right) {
          slider.setValue(slider.value + slider.valueStep)
        } else {
          dragThumbInfo = Some(new DragEventInfo(Complex(x,y), 0, 0, Some(slider)))
        }
      case VerticalBar =>
        if (y < thumb.bottom) {
          slider.setValue(slider.value - slider.valueStep)
        } else if (y > thumb.top) {
          slider.setValue(slider.value + slider.valueStep)
        } else {
          dragThumbInfo = Some(new DragEventInfo(Complex(x,y), 0, 0, Some(slider)))
        }
    }
  }

  /**
   * Aborts any dragging procedure of the thumb.
   */
  def onMouseReleased(x: Double, y: Double): Unit = if (dragThumbInfo.isDefined) dragThumbInfo = None

  /**
   * Moves the thumb if a dragging procedure is launched.
   */
  def onMouseMoved(x: Double, y: Double): Unit = if (dragThumbInfo.isDefined) {
    val slider = dragThumbInfo.get.frame.get.asInstanceOf[Slider]
    val thumb = slider._thumbTexture
    val startX = dragThumbInfo.get.beginPos.re
    val startY = dragThumbInfo.get.beginPos.im

    if (dragThumbInfo.get.alreadyMoved || math.max(math.abs(x - startX), math.abs(y - startY)) > 7) {
      dragThumbInfo.get.alreadyMoved = true

      val (minValue, maxValue) = slider.minMaxValues
      slider.orientation match {
        case HorizontalBar =>
          val length = slider.width - thumb.width
          val left = slider.left + thumb.width / 2
          slider.setValue((maxValue - minValue) / length * (x - left) + minValue)
        case VerticalBar =>
          val length = slider.height - thumb.height
          val bottom = slider.bottom + thumb.height / 2
          slider.setValue((maxValue - minValue) / length * (y - bottom) + minValue)
      }
    }
  }
}