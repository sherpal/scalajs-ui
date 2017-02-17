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
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.raw._



class Painter(val canvas: html.Canvas, val ctx: CanvasRenderingContext2D) {//val gl: WebGLRenderingContext) {

  val renderingCanvas: Canvas2D = new Canvas2D(canvas, ctx)
  renderingCanvas.setBackgroundColor(0,0,0)

  private var currentCanvases: List[CustomCanvas] = List(renderingCanvas)

  def withCanvases(cs: List[CustomCanvas])(body: => Unit): Unit = {
    val prevList = currentCanvases
    currentCanvases = cs
    try body finally currentCanvases = prevList
  }

  def withCanvases(c: CustomCanvas)(body: => Unit): Unit =
    withCanvases(List(c))(body)

  def dimensions: (Int, Int) = (canvas.width, canvas.height)

  def canvasToImage(canvas: HTMLCanvasElement, callback: (HTMLImageElement) => Unit = (_: HTMLImageElement) => {}):
  HTMLImageElement = {
    val image = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
    image.onload = (_: Event) => callback(image)
    image.src = canvas.toDataURL("image/png")
    image
  }

  def imageToCanvas(image: HTMLImageElement): HTMLCanvasElement = {
    val canvas = dom.document.createElement("canvas").asInstanceOf[HTMLCanvasElement]
    canvas.width = image.width
    canvas.height = image.height
    canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D].drawImage(image, 0, 0)
    canvas
  }


  def backgroundColor: Vec4 = renderingCanvas.backgroundColor
  def setBackgroundColor(): Unit = currentCanvases.foreach(_.setBackgroundColor())
  def setBackgroundColor(v: Vec4): Unit = currentCanvases.foreach(_.setBackgroundColor(v))
  def setBackgroundColor(r: Double, g: Double, b: Double): Unit = currentCanvases.foreach(_.setBackgroundColor(r, g, b))

  def drawingColor: Vec4 = renderingCanvas.drawingColor

  def withColor(v: Vec4)(body: => Unit): Unit =
    currentCanvases.foreach(_.withColor(v)(body))
  def withColor(r: Double, g: Double, b: Double, alpha: Double = 1)(body: => Unit): Unit =
    withColor(Vec4(r,g,b,alpha))(body)

  def withScissor(x: Double, y: Double, width: Double, height: Double)(body: => Unit): Unit =
    currentCanvases.foreach(_.withScissor(x, y, width, height)(body))

  def drawRectangle(z: Complex, width: Double, height: Double, color: Vec4 = Vec4(1.0, 1.0, 1.0, 1.0),
                    fill: Boolean = true): Unit =
    currentCanvases.foreach(_.drawRectangle(z, width, height, color, fill))

  def drawLine(vertices: Seq[Complex], color: Vec4 = Vec4(1,1,1,1), lineWidth: Int = 5): Unit =
    currentCanvases.foreach(_.drawLine(vertices, color, lineWidth))

  def drawDisk(center: Complex, radius: Double, color: Vec4 = Vec4(1,1,1,1), segments: Int = 20): Unit =
    currentCanvases.foreach(_.drawDisk(center, radius, color, segments))

  def drawEllipse(center: Complex, xRadius: Double, yRadius: Double, rotation: Double = 0, color: Vec4 = Vec4(1,1,1,1),
                  segments: Int = 20, fill: Boolean = true): Unit =
    currentCanvases.foreach(_.drawEllipse(center, xRadius, yRadius, rotation, color, segments, fill))


  def drawTextureQuad(tex: HTMLImageElement, quad: Quad, topLeft: Complex, width: Double, height: Double): Unit =
    currentCanvases.foreach(_.drawTextureQuad(tex, quad, topLeft, width, height))

  def drawImage(image: HTMLImageElement, topLeft: Complex, width: Double, height: Double): Unit =
    currentCanvases.foreach(_.drawImage(image, topLeft, width, height))

  def drawCanvas(canvas: HTMLCanvasElement, topLeft: Complex, width: Double, height: Double): Unit =
    currentCanvases.foreach(_.drawCanvas(canvas, topLeft, width, height))

  def drawTexture(tex: HTMLImageElement, topLeft: Complex, width: Double, height: Double): Unit =
    currentCanvases.foreach(_.drawTexture(tex, topLeft, width, height))



  private val textCtx: CanvasRenderingContext2D = dom.document
    .createElement("canvas").asInstanceOf[html.Canvas]
    .getContext("2d").asInstanceOf[CanvasRenderingContext2D]
  private val textCanvas: html.Canvas = textCtx.canvas
  textCanvas.width = canvas.width
  textCanvas.height = canvas.height
  textCanvas.style.backgroundColor = "rgba(0,0,0,0)"

  def print(text: String, z: Complex, width: Double, height: Double,
            xOffset: Double, yOffset: Double,
            font: String,
            textAlign: String,
            textBaseLine: String,
            color: String,
            alpha: Double): Unit =
    currentCanvases.foreach(_.print(text, z, width, height, xOffset, yOffset,
      font, textAlign, textBaseLine, color, alpha))

    def print(texts: Seq[(String, String)], z: Complex, width: Double, height: Double,
              xOffset: Double = 0, yOffset: Double = 0,
              font: String = "20px monospace",
              textAlign: String = "left",
              textBaseLine: String = "middle",
              alpha: Double = 1.0): Unit =
      currentCanvases.foreach(_.print(texts, z, width, height, xOffset, yOffset, font, textAlign, textBaseLine, alpha))

//  private def setFont(font: String): Unit = {
//    if (font != textCtx.font)
//      textCtx.font = font
//  }

  def textWidth(text: String, font: String): Double = {
    textCtx.save()
    //setFont(font)
    textCtx.font = font
    val w = textCtx.measureText(text).width
    textCtx.restore()
    w
  }
  def textWidth(c: Char, font: String): Double = {
    textWidth(c.toString, font)
  }

  def clear(): Unit = currentCanvases.foreach(_.clear())

  /////////////////////////////////////////////////////////////////////////////////////////////////////////
  /// Transformation matrix manipulations ///
  ///////////////////////////////////////////

  def resetTransformationMatrix(): Unit =
    currentCanvases.foreach(_.resetTransformationMatrix())

  def rotate(angle: Double, dim: Int = 3): Unit =
    currentCanvases.foreach(_.rotate(angle, dim))

  def scale(sx: Double, sy: Double, sz: Double = 1): Unit =
    currentCanvases.foreach(_.scale(sx, sy, sz))

  def translate(dx: Double, dy: Double, dz: Double = 0): Unit =
    currentCanvases.foreach(_.translate(dx, dy, dz))

  def storeTransformationMatrix(): Unit =
    currentCanvases.foreach(_.storeTransformationMatrix())

  def restoreTransformationMatrix(): Unit =
    currentCanvases.foreach(_.restoreTransformationMatrix())




}


