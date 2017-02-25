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
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.raw.Event
import webglgraphics.{CustomCanvas, Quad}


class Texture(n: String = "", p: Frame) extends LayeredRegion {

  val name: String = UIObject.setName(n)
  protected var _parent: Option[Frame] = None
  setParent(p)


  private var _xFlip: Int = 1
  private var _yFlip: Int = 1
  def xFlip: Double = _xFlip
  def yFlip: Double = _yFlip
  def setXFlip(flip: Boolean = false): Unit = _xFlip = if (flip) -1 else 1
  def setYFlip(flip: Boolean = false): Unit = _yFlip = if (flip) -1 else 1

  private var _mode: DrawMode = FillMode
  def mode: DrawMode = _mode
  def setMode(mode: DrawMode = FillMode): Unit = _mode = mode

  var lineWidth: Int = 2

  private var _rotation: Double = 0
  def rotation: Double = _rotation
  def setRotation(rotation: Double = 0): Unit = _rotation = rotation

  private var _quad: Option[Quad] = None//Quad(0,1,1,0,1,1)
  def quad: Option[Quad] = _quad
  def setTexCoord(x: Double, y: Double, width: Double, height: Double, imageWidth: Double, imageHeight: Double): Unit =
    _quad = Some(Quad(x, x + width, y + height, y, imageWidth, imageHeight))
  def setTexCoord(): Unit = _quad = None//Quad(0,1,1,0,1,1)


  private var _texture: Option[html.Image] = None

  def texture: Option[html.Image] = _texture
  def setTexture(tex: Option[html.Image] = None): Unit = {
    _canvas = None
    _texture = tex
  }
  def setTexture(file: String): Unit = {
    _canvas = None
    val image = dom.document.createElement("img").asInstanceOf[html.Image]
    image.onload = (_: Event) => {
      setTexture(Some(image))
    }
    image.src = file
  }
  def setTexture(canvas: html.Canvas): Unit = {
    _canvas = None
    val image = dom.document.createElement("img").asInstanceOf[html.Image]
    image.onload = (_: Event) => setTexture(Some(image))
    image.src = canvas.toDataURL("image.png")
  }
  def setTexture(canvas: CustomCanvas): Unit = {
    _canvas = Some(canvas)
    _texture = Some(canvas.canvas.asInstanceOf[html.Image])
  }

  private var _canvas: Option[CustomCanvas] = None
  def canvas: Option[CustomCanvas] = _canvas


  // TODO: allow tex coords for quad drawing

  def vertexColor: (Double, Double, Double, Double) = (_red, _green, _blue, alpha)


  def isImage: Boolean = _texture.isDefined

  def removeImage(): Unit = _texture = None



  def draw(): Unit = if (isVisible) {
    def rawDraw(): Unit = {
      _texture match {
        case Some(tex) =>
          _quad match {
            case Some(quad) => Engine.painter.drawTextureQuad(tex, quad, Complex(left, top), width, height)
            case None => Engine.painter.drawTexture(tex, Complex(left, top), width, height)
          }

        case _ =>
          if (isRectangle) {
            Engine.painter.drawRectangle(Complex(left, top), width, height,
              lineWidth = if (mode == FillMode) 0 else lineWidth)
          } else if (isDisk) {
            Engine.painter.drawDisk(center, radius)
          }
      }
    }

    Engine.painter.withColor(_red, _green, _blue, getEffectiveAlpha)({
      _scrollFrameAncestor match {
        case Some(scrollFrame) =>
          Engine.painter.withScissor(scrollFrame.left.toInt, scrollFrame.bottom.toInt,
            scrollFrame.width.toInt, scrollFrame.height.toInt)(rawDraw())
        case _ =>
          rawDraw()
      }
    })
  }
}


abstract sealed class DrawMode
case object LineMode extends DrawMode
case object FillMode extends DrawMode
