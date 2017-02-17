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

package webglgraphics

import complex.Complex
import org.scalajs.dom.html

/**
 * A CustomCanvas is used to display things on the screen. It encapsulates a [[html.Canvas]] that is actually used to
 * draw.
 * A CustomCanvas is a "contract" Trait that should be followed. A class inheriting from CustomCanvas should not have
 * any additional methods.
 */
trait CustomCanvas {

  val canvas: html.Canvas

  /** Returns the width of the [[html.Canvas]] element attached to the Canvas. */
  def width: Int = canvas.width

  /** Sets the width of the [[html.Canvas]] element attached to the Canvas. */
  def setWidth(width: Int): Unit =
    canvas.width = width

  /** Returns the height of the [[html.Canvas]] element attached to the Canvas. */
  def height: Int = canvas.height

  /** Sets the height of the [[html.Canvas]] element attached to the Canvas. */
  def setHeight(height: Int): Unit =
    canvas.height = height

  /** Sets the width and height of the [[html.Canvas]] element attached to the Canvas. */
  def setSize(width: Int, height: Int): Unit = {
    setWidth(width)
    setHeight(height)
  }



  private var _backgroundColor: Vec4 = Vec4(0,0,0,0)
  def backgroundColor: Vec4 = _backgroundColor
  def setBackgroundColor(): Unit =
    setBackgroundColor(0,0,0,0)
  def setBackgroundColor(v: Vec4): Unit =
    _backgroundColor = v
  def setBackgroundColor(r: Double, g: Double, b: Double, a: Double = 1.0): Unit =
    setBackgroundColor(Vec4(r,g,b,a))

  private var _drawingColor: Vec4 = Vec4(1,1,1,1)
  def drawingColor: Vec4 = _drawingColor

  def withColor[A](v: Vec4)(body: => A): A = {
    val previousColor = drawingColor
    _drawingColor = v
    try body finally _drawingColor = previousColor
  }

  /**
   * Executes body with scissor set to rectangle of bottom left corner x+iy.
   */
  def withScissor[A](x: Double, y: Double, width: Double, height: Double)(body: => A): A

  /**
   * Draws a rectangle of the given color.
   * The color is multiplied by the drawingColor property.
   *
   * @param z      top left pixel of the rectangle area.
   * @param width  width of the rectangle.
   * @param height height of the rectangle.
   * @param color  css color of the region to draw.
   * @param fill   whether the region should be filled.
   */
  def drawRectangle(z: Complex, width: Double, height: Double, color: Vec4 = Vec4(1.0, 1.0, 1.0, 1.0),
    fill: Boolean = true): Unit

  /**
   * Draw a circle of the given color.
   * The color is multiplied by the drawingColor property.
   *
   * @param center   the center of the disk, in pixels.
   * @param radius   the radius of the disk, in pixels.
   * @param color    color of the region to draw in rgba, in [0,1].
   * @param segments number of segments used to draw the circle. Meaningless for [[Canvas2D]].
   * @param fill     whether the region should be filled.
   */
  def drawDisk(center: Complex, radius: Double, color: Vec4 = Vec4(1,1,1,1),
               segments: Int = 20, fill: Boolean = true): Unit

  def drawEllipse(center: Complex, xRadius: Double, yRadius: Double, rotation: Double = 0, color: Vec4 = Vec4(1,1,1,1),
                  segments: Int = 20, fill: Boolean = true): Unit

  def drawLine(vertices: Seq[Complex], color: Vec4 = Vec4(1,1,1,1), lineWidth: Int = 5): Unit

  def drawVertices(vertices: Seq[Complex], color: Vec4, mode: String = "fill"): Unit

  def drawTextureQuad(tex: html.Image, quad: Quad, topLeft: Complex, width: Double, height: Double): Unit

  def drawImage(image: html.Image, topLeft: Complex, width: Double, height: Double): Unit

  def drawCanvas(canvas: html.Canvas, topLeft: Complex, width: Double, height: Double): Unit

  def drawTexture(tex: html.Image, topLeft: Complex, width: Double, height: Double): Unit

  def print(text: String, z: Complex, width: Double, height: Double,
            xOffset: Double, yOffset: Double,
            font: String,
            textAlign: String,
            textBaseLine: String,
            color: String,
            alpha: Double): Unit =
    print(List((text, color)), z, width, height,
      xOffset = xOffset, yOffset = yOffset,
      font = font, textAlign = textAlign, textBaseLine = textBaseLine,
      alpha = alpha)

  def print(texts: Seq[(String, String)], z: Complex, width: Double, height: Double,
            xOffset: Double = 0, yOffset: Double = 0,
            font: String = "20px monospace",
            textAlign: String = "left",
            textBaseLine: String = "middle",
            alpha: Double = 1.0): Unit

  def textWidth(text: String, font: String): Double
  def textWidth(c: Char, font: String): Double

  def clear(): Unit


  def resetTransformationMatrix(): Unit
  def rotate(angle: Double, dim: Int = 3): Unit
  def scale(sx: Double, sy: Double, sz: Double = 1): Unit
  def translate(dx: Double, dy: Double, dz: Double = 0): Unit
  def storeTransformationMatrix(): Unit
  def restoreTransformationMatrix(): Unit

}
