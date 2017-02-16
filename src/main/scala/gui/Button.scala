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
import gameengine.Engine
import webglgraphics.Canvas2D


class Button(n: String = "", par: Option[Frame] = Some(UIParent)) extends Frame(n, par) with Focusable with Textable {
  private var _state: ButtonState = Normal

  private val _text: FontString = createFontString(name + "text")
  _text.setAllPoints()
  def text: String = _text.text
  def setText(text: String): Unit = _text.setText(text)
  def textChild: FontString = _text
  def setTextColor(r: Double = 1,
                   g: Double = 1,
                   b: Double = 1,
                   alpha: Double = 1): Unit = _text.setTextColor(r, g, b, alpha)

  private var _highlightTexture: Option[Texture] = None
  def setHighlightTexture(tex: Texture): Unit = {
    if (scala.scalajs.LinkingInfo.developmentMode)
      assert(tex.parent.isDefined && tex.parent.get == this, "Texture is not a child of button")

    _highlightTexture = Some(tex)
    _highlightTexture.get.setDrawLayer(Highlight)
    _highlightTexture.get.setAllPoints()
    _highlightTexture.get.hide()
  }
  def setHighlightTexture(): Unit = {
    setHighlightTexture(createTexture())
  }
  def setHighlightTexture(file: String): Unit = {
    setHighlightTexture(createTexture())
    _highlightTexture.get.setTexture(file)
  }
  def highlightTexture: Option[Texture] = _highlightTexture
  def showHighlight(): Unit = {
    _highlightTexture match {
      case Some(tex) =>
        tex.show()
      case _ =>
    }
  }
  def hideHighlight(): Unit = _highlightTexture match {
    case Some(tex) => tex.hide()
    case _ =>
  }

  private var _pushedTexture: Option[Texture] = None
  def setPushedTexture(tex: Texture): Unit = {
    if (scala.scalajs.LinkingInfo.developmentMode)
      assert(tex.parent.isDefined && tex.parent.get == this, "Texture is not a child of button")

    _pushedTexture = Some(tex)
  }
  def setPushedTexture(): Unit = setPushedTexture(createTexture())
  def setPushedTexture(file: String): Unit = {
    setPushedTexture()
    _pushedTexture.get.setTexture(file)
  }
  def pushedTexture: Option[Texture] = _pushedTexture
  def showPushedTexture(): Unit = _pushedTexture match {
    case Some(tex) =>
      tex.show()
      hideNormalTexture()
      hideDisabledTexture()
    case _ =>
  }
  def hidePushedTexture(): Unit = _pushedTexture match {
    case Some(tex) => tex.hide()
    case _ =>
  }

  private var _disabledTexture: Option[Texture] = None
  def setDisabledTexture(tex: Texture): Unit = {
    if (scala.scalajs.LinkingInfo.developmentMode)
      assert(tex.parent.isDefined && tex.parent.get == this, "Texture is not a child of button")

    _disabledTexture = Some(tex)
  }
  def setDisabledTexture(): Unit = setDisabledTexture(createTexture())
  def setDisabledTexture(file: String): Unit = {
    setDisabledTexture()
    _disabledTexture.get.setTexture(file)
  }
  def disabledTexture: Option[Texture] = _disabledTexture
  def showDisabledTexture(): Unit = _disabledTexture match {
    case Some(tex) =>
      tex.show()
      hideNormalTexture()
      hidePushedTexture()
      hideHighlight()
    case _ =>
      hideHighlight()
  }
  def hideDisabledTexture(): Unit = _disabledTexture match {
    case Some(tex) => tex.hide()
    case _ =>
  }

  private var _normalTexture: Option[Texture] = None
  def setNormalTexture(tex: Texture): Unit = {
    if (scala.scalajs.LinkingInfo.developmentMode)
      assert(tex.parent.isDefined && tex.parent.get == this, "Texture is not a child of button")

    _normalTexture = Some(tex)
    if (_state == Normal) _normalTexture.get.show()
  }
  def setNormalTexture(): Unit = setNormalTexture(createTexture())
  def setNormalTexture(file: String): Unit = {
    setNormalTexture()
    _normalTexture.get.setTexture(file)
  }
  def normalTexture: Option[Texture] = _normalTexture
  def showNormalTexture(): Unit = _normalTexture match {
    case Some(tex) =>
      tex.show()
      hidePushedTexture()
      hideDisabledTexture()
    case _ =>
  }
  def hideNormalTexture(): Unit = _normalTexture match {
    case Some(tex) => tex.hide()
    case _ =>
  }


  def isEnabled: Boolean = _state != Disabled
  def enable(): Unit = if (!isEnabled) setState(Normal)
  def disable(): Unit = setState(Disabled)


  def buttonState: ButtonState = _state
  def setState(state: ButtonState): Unit = {
    _state = state
    state match {
      case Normal => showNormalTexture()
      case Pushed => showPushedTexture()
      case Disabled => showDisabledTexture()
    }
  }


  def click(x: Double, y: Double, button: Int, down: Boolean = true): Unit = {
    if (down) {
      fires[Frame, Double, Double, Int, Unit](ScriptKind.OnClick)(x, y, button)
    } else {
      fires[Frame, Double, Double, Int, Unit](ScriptKind.OnMouseReleased)(x, y, button)
    }
  }


}


object Button {
  def makeSimplePushedTexture(button: Button): Texture = {
    button.normalTexture match {
      case None => button.createTexture()
      case Some(normalTex) =>
        val pushedTex = button.createTexture()

        pushedTex.setAllPoints()
        pushedTex.hide()

        val canvas = new Canvas2D()
        canvas.setSize(normalTex.width.toInt, normalTex.height.toInt)

        val (red, green, blue, alpha) = normalTex.vertexColor

        Engine.painter.setCanvas(canvas)

        Engine.painter.setColor(red * 2.0 / 3, green * 2.0 / 3, blue * 2.0 / 3, alpha * 2.0 / 3)

        normalTex.texture match {
          case Some(tex) =>
            val (width, height) = normalTex.size
            normalTex.quad match {
              case Some(quad) =>
                Engine.painter.drawTextureQuad(tex, quad, -width/2 + Complex.i * height / 2, width, height)
              case None =>
                Engine.painter.drawTexture(tex, Complex(-width / 2, height / 2 - 2), width, height)
            }

          case None =>
            if (normalTex.isRectangle) {
              Engine.painter.drawRectangle(- normalTex.width / 2 + (normalTex.height / 2 - 2) * Complex.i,
                normalTex.width, normalTex.height)
            } else if (normalTex.isDisk) {
              Engine.painter.drawDisk(0, normalTex.radius)
            }
        }
        Engine.painter.setCanvas()

        pushedTex.setTexture(canvas.canvas)
        pushedTex
    }
  }
}



abstract sealed class ButtonState
case object Normal extends ButtonState
case object Disabled extends ButtonState
case object Pushed extends ButtonState
