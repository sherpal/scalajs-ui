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

import Matrix.Matrix
import complex.Complex
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.raw.{CanvasRenderingContext2D, HTMLImageElement}


/**
 * Canvas objects are use to draw stuff on them.
 * The [[html.Canvas]] element created in the html file for actually printing on the screen.
 * This class uses the CanvasRenderingContext2D technology to draw on the canvas.
 */
class Canvas2D(val canvas: html.Canvas, ctx: CanvasRenderingContext2D) extends CustomCanvas {

  /**
   * Creates an instance of Canvas for a pre-existing html.Canvas element.
   * /!\ Doing this will bind the context to the canvas, so it can't be done either before or after.
   */
  def this(canvas: html.Canvas) = this(canvas, canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D])

  /**
   * Creates an instance of Canvas with a newly created html.Canvas element.
   * /!\ This automatically bind the context to the canvas, so it can't be done after creation.
   */
  def this() = this(dom.document.createElement("canvas").asInstanceOf[html.Canvas])


  private def changeCoordinates(z: Complex): (Double, Double) = (z.re + canvas.width / 2, canvas.height / 2 - z.im)

  /**
   * Sets a rectangle area that restrain the drawing area.
   * We go from cartesian coordinates ((0,0) is at the center of the canvas and y go up) to canvas coordinates
   * ((0,0) at the top left and y go down).
   *
   * @param x      left pixel of area.
   * @param y      top pixel of area.
   * @param width  width of area, in pixels.
   * @param height height of area, in pixels.
   */
  def setScissor(x: Double, y: Double, width: Double, height: Double): Unit = {
    val (locX, locY) = changeCoordinates(Complex(x, y + height))//x + Complex.i * (y + height))
    ctx.save()
    ctx.beginPath()
    ctx.rect(locX, locY, width, height)
    ctx.clip()
  }

  /** Removes the scissor area */
  def setScissor(): Unit =
    ctx.restore()

  def drawRectangle(z: Complex, width: Double, height: Double, color: Vec4 = Vec4(1.0, 1.0, 1.0, 1.0),
                    fill: Boolean = true): Unit = {
    if (fill) {
      ctx.fillStyle = (color * drawingColor).toCSSColor
      val (locX, locY) = changeCoordinates(z)
      ctx.fillRect(locX, locY, width, height)
    } else {
      ctx.lineWidth = 2
      ctx.strokeStyle = (color * drawingColor).toCSSColor
      val (locX, locY) = changeCoordinates(z)
      ctx.beginPath()
      ctx.rect(locX, locY, width, height)
      ctx.stroke()
    }
  }

  def drawDisk(center: Complex, radius: Double, color: Vec4 = Vec4(1,1,1,1),
               segments: Int = 20, fill: Boolean = true): Unit = {
    ctx.beginPath()
    val (locX, locY) = changeCoordinates(center)
    ctx.arc(locX, locY, radius, 0, 2 * math.Pi)
    if (fill) {
      ctx.fillStyle = (color * drawingColor).toCSSColor
      ctx.fill()
    } else {
      ctx.strokeStyle = (color * drawingColor).toCSSColor
      ctx.stroke()
    }
  }

  def drawEllipse(center: Complex, xRadius: Double, yRadius: Double, rotation: Double = 0, color: Vec4 = Vec4(1,1,1,1),
                  segments: Int = 20, fill: Boolean = true): Unit = {
    val vertices = (for (j <- 0 to segments) yield
      center + xRadius * math.cos(j * 2 * math.Pi / segments) + Complex.i * (
        yRadius * math.sin(j * 2 * math.Pi / segments)
        )).toVector
    drawVertices(vertices, color, if (fill) "fill" else "line")
  }

  def drawLine(vertices: Seq[Complex], color: Vec4 = Vec4(1,1,1,1), lineWidth: Int = 2): Unit = {
    ctx.lineWidth = lineWidth
    drawVertices(vertices, color, "line")
  }

  def drawVertices(vertices: Seq[Complex], color: Vec4, mode: String = "fill"): Unit =
    if (vertices.nonEmpty && vertices.tail.nonEmpty){
    val canvasSpaceVertices = vertices.map(changeCoordinates)
    ctx.beginPath()
    ctx.moveTo(canvasSpaceVertices.head._1, canvasSpaceVertices.head._2)
    canvasSpaceVertices.tail.foreach({case (x, y) => ctx.lineTo(x, y)})
    if (mode == "fill") {
      ctx.closePath()
      ctx.fillStyle = (color * drawingColor).toCSSColor
      ctx.fill()
    } else {
      ctx.lineWidth = 2
      ctx.strokeStyle = (color * drawingColor).toCSSColor
      ctx.stroke()
    }
  }

  def drawTextureQuad(tex: html.Image, quad: Quad, topLeft: Complex, width: Double, height: Double): Unit = {
    val (dx, dy) = changeCoordinates(topLeft)
    val (sx, sy) = (quad.left, quad.imageHeight - quad.top)
    val (sWidth, sHeight) = (quad.right - quad.left, quad.top - quad.bottom)
    ctx.drawImage(tex, sx, sy, sWidth, sHeight, dx, dy, width, height)
  }

  def drawImage(image: HTMLImageElement, topLeft: Complex, width: Double, height: Double): Unit = {
    val (dx, dy) = changeCoordinates(topLeft)
    ctx.drawImage(image, dx, dy, width, height)
  }

  def drawCanvas(canvas: html.Canvas, topLeft: Complex, width: Double, height: Double): Unit = {
    val (dx, dy) = changeCoordinates(topLeft)
    ctx.drawImage(canvas, dx, dy, width, height)
  }

  def drawTexture(tex: html.Image, topLeft: Complex, width: Double, height: Double): Unit = {
    val (dx, dy) = changeCoordinates(topLeft)
    ctx.drawImage(tex, dx, dy, width, height)
  }

  private var registeredFont = ctx.font
  private def setFont(font: String): Unit = if (font != registeredFont) {
    ctx.font = font
    registeredFont = font
  }

  def print(texts: Seq[(String, String)], z: Complex, width: Double, height: Double,
            xOffset: Double = 0, yOffset: Double = 0,
            font: String = "20px monospace",
            textAlign: String = "left",
            textBaseLine: String = "middle",
            alpha: Double = 1.0): Unit = {
    ctx.font = font
    ctx.textAlign = textAlign
    ctx.textBaseline = textBaseLine

    var left: Double = 0
    val (x,y) = changeCoordinates(z)
    val (xOff, yOff) = (xOffset, yOffset) // yOffset grows when text must be lower
    texts.foreach({case (text, color) =>
      ctx.fillStyle = color
      ctx.fillText(text, x + xOff + left, y + yOff)
      left += ctx.measureText(text).width
    })
  }

  def textWidth(text: String, font: String): Double = {
    setFont(font)
    ctx.measureText(text).width
  }

  def textWidth(c: Char, font: String): Double = textWidth(c.toString, font)


  def clear(): Unit = {
    setScissor()
    ctx.clearRect(0, 0, canvas.width, canvas.height)
    ctx.fillStyle = backgroundColor.toCSSColor
    ctx.fillRect(0, 0, canvas.width, canvas.height)
  }

  // transformation matrix
  private val eye4: Matrix = Matrix.eye(4)
  private var _transformationMatrix: Matrix = eye4
  private var _storedTransformationMatrix: Matrix = eye4

  def resetTransformationMatrix(): Unit = {
    _transformationMatrix = eye4
    ctx.setTransform(1,0,0,1,0,0)
  }

  /**
   * Rotates the shapes of angle radians in a counterclockwise direction.
   *
   * @param angle angle we want to rotate the shape, in radians.
   * @param dim   dim can only be 3 for Canvas2D.
   */
  def rotate(angle: Double, dim: Int = 3): Unit = {
    // need -angle since CanvasRenderingContext2D takes clockwise direction (which is non sense, by the way).
    _transformationMatrix = Matrix.zRotation3d(-angle) * _transformationMatrix
    setTransform()
  }

  /** Scales according to x and y. The z scaling is always 1 for 2d canvases. */
  def scale(sx: Double, sy: Double, sz: Double = 1): Unit = {
    _transformationMatrix = Matrix.scaling3d(sx, sy, 1) * _transformationMatrix
    setTransform()
  }

  /** Translates the origin by dx, dy. The z translation is always 0 for 2d canvases. */
  def translate(dx: Double, dy: Double, dz: Double = 0): Unit = {
    _transformationMatrix = Matrix.translation3d(dx, dy, 0) * _transformationMatrix
    setTransform()
  }

  /** Stores the transformation matrix. */
  def storeTransformationMatrix(): Unit = {
    _storedTransformationMatrix = _transformationMatrix
  }

  /** Sets the transformation matrix to the stored matrix. */
  def restoreTransformationMatrix(): Unit = {
    _transformationMatrix = _storedTransformationMatrix
    setTransform()
  }

  private def setTransform(): Unit = {
    ctx.setTransform(
      _transformationMatrix(0,0), _transformationMatrix(0,1),
      _transformationMatrix(1,0), _transformationMatrix(1,1),
      _transformationMatrix(0,3), _transformationMatrix(1,3)
    )
  }

}
