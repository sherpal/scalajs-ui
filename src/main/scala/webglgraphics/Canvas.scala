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

import matrix.Matrix
import complex.Complex
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.raw._

import scala.scalajs.js.JSConverters._
import scala.scalajs.js.typedarray.Float32Array

/**
 * Canvas objects are use to draw stuff on them.
 * The [[html.Canvas]] element created in the html file for actually printing on the screen.
 * This class uses the webgl technology to draw on the canvas.
 *
 * /!\ This is currently abandoned and does not work correctly.
 */
class Canvas(val canvas: html.Canvas, gl: WebGLRenderingContext) extends CustomCanvas {

  private val i = Complex.i

  /**
   * Creates an instance of Canvas for a pre-existing html.Canvas element.
   * /!\ Doing this will bind the webgl context to the canvas, so it can't be done either before or after.
   */
  def this(canvas: html.Canvas) = this(canvas, canvas.getContext("webgl").asInstanceOf[WebGLRenderingContext])

  /**
   * Creates an instance of Canvas with a newly created html.Canvas element.
   * /!\ This automatically bind the webgl context to the canvas, so it can't be done after creation.
   */
  def this() = this(dom.document.createElement("canvas").asInstanceOf[html.Canvas])


//  /** Returns the width of the [[html.Canvas]] element attached to the Canvas. */
//  def width: Int = canvas.width
//
//  /** Sets the width of the [[html.Canvas]] element attached to the Canvas. */
//  def setWidth(width: Int): Unit =
//    canvas.width = width
//
//  /** Returns the height of the [[html.Canvas]] element attached to the Canvas. */
//  def height: Int = canvas.height
//
//  /** Sets the height of the [[html.Canvas]] element attached to the Canvas. */
//  def setHeight(height: Int): Unit =
//    canvas.height = height
//
//  /** Sets the width and height of the [[html.Canvas]] element attached to the Canvas. */
//  def setSize(width: Int, height: Int): Unit = {
//    setWidth(width)
//    setHeight(height)
//  }
//

  /*================ Shaders ====================*/

  private var _lastProgram: WebGLProgram = _
  private def useProgram(program: WebGLProgram): Unit = if (program != _lastProgram) {
    gl.useProgram(program)
    _lastProgram = program
  }


  // Create an empty buffer object to store vertex buffer
  private val vertexBuffer: WebGLBuffer = gl.createBuffer()

  // Vertex shader source code
  private val vertexCode: String = """
                                     |attribute vec3 coordinates;
                                     |
                                     |uniform mat4 transformation_matrix;
                                     |
                                     |void main(void) {
                                     |gl_Position = transformation_matrix * vec4(coordinates, 1.0);
                                     |}
                                   """.stripMargin
  // Create a vertex shader object
  private val vertexShader: WebGLShader = gl.createShader(WebGLRenderingContext.VERTEX_SHADER)
  // Attach vertex shader source code
  gl.shaderSource(vertexShader, vertexCode)
  // Compile the vertex shader
  gl.compileShader(vertexShader)
  if (scala.scalajs.LinkingInfo.developmentMode) {
    if (!gl.getShaderParameter(vertexShader, WebGLRenderingContext.COMPILE_STATUS).asInstanceOf[Boolean]) {
      println("Error while compiling shader")
      println(gl.getShaderInfoLog(vertexShader))
    }
  }


  // Fragment shader source code
  private val fragmentCode: String = """
                                       |precision mediump float;
                                       |
                                       |uniform vec4 painter_color;
                                       |uniform vec4 u_color;
                                       |
                                       |void main(void) {
                                       |gl_FragColor = u_color * painter_color;
                                       |}""".stripMargin

  // Create fragment shader object
  private val fragmentShader: WebGLShader = gl.createShader(WebGLRenderingContext.FRAGMENT_SHADER)

  // Attach fragment shader source code
  gl.shaderSource(fragmentShader, fragmentCode)

  // Compile the fragment shader
  gl.compileShader(fragmentShader)

  if (scala.scalajs.LinkingInfo.developmentMode) {
    if (!gl.getShaderParameter(fragmentShader, WebGLRenderingContext.COMPILE_STATUS).asInstanceOf[Boolean]) {
      println("Error while compiling shader")
      println(gl.getShaderInfoLog(fragmentShader))
    }
  }

  // Create a shader program object to store the combined shader object
  private val shaderProgram: WebGLProgram = gl.createProgram()

  // Attach a vertex shader
  gl.attachShader(shaderProgram, vertexShader)
  // Attach a fragment shader
  gl.attachShader(shaderProgram, fragmentShader)
  // Link both the programs
  gl.linkProgram(shaderProgram)

  // Get the attribute location
  private val coordLocation: Int = gl.getAttribLocation(shaderProgram, "coordinates")
  private val paintingColorLocation: WebGLUniformLocation = gl.getUniformLocation(shaderProgram, "painter_color")
  private val transformationMatrixLocation: WebGLUniformLocation =
    gl.getUniformLocation(shaderProgram, "transformation_matrix")
  private val customColorLocation: WebGLUniformLocation = gl.getUniformLocation(shaderProgram, "u_color")


  //////////////////////////////////////////////////////////////////////////////////////////////////////////
  private val textureVertexCode: String = """
                                          |attribute vec3 coordinates;
                                          |attribute vec2 a_tex_coord;
                                          |
                                          |uniform mat4 transformation_matrix;
                                          |
                                          |varying vec2 v_tex_coord;
                                          |
                                          |void main() {
                                          |gl_Position = transformation_matrix * vec4(coordinates, 1.0);
                                          |
                                          |// pass the texcoord to the fragment shader
                                          |v_tex_coord = a_tex_coord;
                                          |}
                                        """.stripMargin

  // Create a vertex shader object
  private val textureVertexShader: WebGLShader = gl.createShader(WebGLRenderingContext.VERTEX_SHADER)
  // Attach vertex shader source code
  gl.shaderSource(textureVertexShader, textureVertexCode)
  // Compile the vertex shader
  gl.compileShader(textureVertexShader)
  if (scala.scalajs.LinkingInfo.developmentMode) {
    if (!gl.getShaderParameter(textureVertexShader, WebGLRenderingContext.COMPILE_STATUS).asInstanceOf[Boolean]) {
      println("Error while compiling shader")
      println(gl.getShaderInfoLog(textureVertexShader))
    }
  }

  private val textureFragmentCode: String =
    """
      |precision mediump float;
      |
      |// Passed in from the vertex shader.
      |varying vec2 v_tex_coord;
      |
      |// general painter_color that changes every color drawn
      |uniform vec4 painter_color;
      |// The texture.
      |uniform sampler2D u_texture;
      |
      |void main() {
      |gl_FragColor = texture2D(u_texture, v_tex_coord) * painter_color;
      |}
    """.stripMargin

  // Create a vertex shader object
  private val textureFragmentShader: WebGLShader = gl.createShader(WebGLRenderingContext.FRAGMENT_SHADER)
  // Attach vertex shader source code
  gl.shaderSource(textureFragmentShader, textureFragmentCode)
  // Compile the vertex shader
  gl.compileShader(textureFragmentShader)
  if (scala.scalajs.LinkingInfo.developmentMode) {
    if (!gl.getShaderParameter(textureFragmentShader, WebGLRenderingContext.COMPILE_STATUS).asInstanceOf[Boolean]) {
      println("Error while compiling shader")
      println(gl.getShaderInfoLog(textureFragmentShader))
    }
  }

  // Create a shader program object to store the combined shader object
  private val textureShaderProgram: WebGLProgram = gl.createProgram()

  // Attach a vertex shader
  gl.attachShader(textureShaderProgram, textureVertexShader)
  // Attach a fragment shader
  gl.attachShader(textureShaderProgram, textureFragmentShader)
  // Link both the programs
  gl.linkProgram(textureShaderProgram)

  private val texCoordBuffer: WebGLBuffer = gl.createBuffer()
  private val texturePositionBuffer: WebGLBuffer = gl.createBuffer()

  // create the texture
  private val webGLTexture: WebGLTexture = gl.createTexture()

  // retrieve place where vertex data should go
  private val positionLocation: Int = gl.getAttribLocation(textureShaderProgram, "coordinates")
  private val texCoordLocation: Int = gl.getAttribLocation(textureShaderProgram, "a_tex_coord")

  // retrieve uniform variable locations
  private val textureTransformationMatrixLocation: WebGLUniformLocation =
    gl.getUniformLocation(textureShaderProgram, "transformation_matrix")
  private val painterColorLocation: WebGLUniformLocation = gl.getUniformLocation(textureShaderProgram, "painter_color")


  // transformation matrix
  private val eye4: Matrix = Matrix.eye(4)
  private var _transformationMatrix: Matrix = eye4
  private var _storedTransformationMatrix: Matrix = eye4

  //private var _backgroundColor: Vec3 = Vec3(0.0, 0.0, 0.0)
  //def backgroundColor: Vec3 = _backgroundColor
//  def setBackgroundColor(): Unit = {
//    _backgroundColor = Vec3(0, 0, 0)
//    gl.clearColor(0,0,0,1)
//  }
//  def setBackgroundColor(v: Vec3): Unit = {
//    _backgroundColor = v
//  }
  //def setBackgroundColor(r: Double, g: Double, b: Double): Unit = setBackgroundColor(Vec3(r,g,b))

//  private var _drawingColor: Vec4 = Vec4(1,1,1,1)
//  def drawingColor: Vec4 = _drawingColor
//  def setColor(): Unit = _drawingColor = Vec4(1,1,1,1)
//  def setColor(v: Vec4): Unit = _drawingColor = v
//  def setColor(r: Double, g: Double, b: Double, a: Double = 1.0): Unit = _drawingColor = Vec4(r, g, b, a)


  // we go from cartesian coordinates ((0,0) is at the center of the canvas and y go up) to canvas coordinates
  // ((0,0) at bottom left corner and y go up)
//  def setScissor(x: Double, y: Double, width: Double, height: Double): Unit = {
//    gl.enable(WebGLRenderingContext.SCISSOR_TEST)
//    gl.scissor((x + canvas.width / 2).toInt, (y + canvas.height / 2).toInt, width.toInt, height.toInt)
//  }
//  def setScissor(): Unit =
//    gl.disable(WebGLRenderingContext.SCISSOR_TEST)

  def withScissor[A](x: Double, y: Double, width: Double, height: Double)(body: => A): A = {
    gl.enable(WebGLRenderingContext.SCISSOR_TEST)
    gl.scissor((x + canvas.width / 2).toInt, (y + canvas.height / 2).toInt, width.toInt, height.toInt)
    try body finally gl.disable(WebGLRenderingContext.SCISSOR_TEST)
  }


  private val rectangleVertexCode =
    """
      |attribute vec3 coordinates;
      |
      |uniform mat4 u_transformation_matrix;
      |uniform mat4 u_rectangle_spec;
      |
      |void main(void) {
      |gl_Position = u_transformation_matrix * u_rectangle_spec * vec4(coordinates, 1.0);
      |}
      |""".stripMargin
  // Create a vertex shader object
  private val rectVertexShader: WebGLShader = gl.createShader(WebGLRenderingContext.VERTEX_SHADER)
  // Attach vertex shader source code
  gl.shaderSource(rectVertexShader, rectangleVertexCode)
  // Compile the vertex shader
  gl.compileShader(rectVertexShader)
  if (scala.scalajs.LinkingInfo.developmentMode) {
    if (!gl.getShaderParameter(rectVertexShader, WebGLRenderingContext.COMPILE_STATUS).asInstanceOf[Boolean]) {
      println("Error while compiling shader")
      println(gl.getShaderInfoLog(rectVertexShader))
    }
  }
  // Fragment shader source code
  private val rectangleFragmentCode: String = """
                                                |precision mediump float;
                                                |
                                                |uniform vec4 painter_color;
                                                |uniform vec4 u_color;
                                                |
                                                |void main(void) {
                                                |gl_FragColor = u_color * painter_color;
                                                |}""".stripMargin

  // Create fragment shader object
  private val rectFragmentShader: WebGLShader = gl.createShader(WebGLRenderingContext.FRAGMENT_SHADER)
  // Attach fragment shader source code
  gl.shaderSource(rectFragmentShader, rectangleFragmentCode)

  // Compile the fragment shader
  gl.compileShader(rectFragmentShader)

  if (scala.scalajs.LinkingInfo.developmentMode) {
    if (!gl.getShaderParameter(rectFragmentShader, WebGLRenderingContext.COMPILE_STATUS).asInstanceOf[Boolean]) {
      println("Error while compiling shader")
      println(gl.getShaderInfoLog(rectFragmentShader))
    }
  }

  // Create a shader program object to store the combined shader object
  private val rectShaderProgram: WebGLProgram = gl.createProgram()

  // Attach a vertex shader
  gl.attachShader(rectShaderProgram, rectVertexShader)
  // Attach a fragment shader
  gl.attachShader(rectShaderProgram, rectFragmentShader)
  // Link both the programs
  gl.linkProgram(rectShaderProgram)

  // Get the attribute location
  private val rectCoordLocation: Int = gl.getAttribLocation(rectShaderProgram, "coordinates")
  private val rectPaintingColorLocation: WebGLUniformLocation = gl.getUniformLocation(
    rectShaderProgram, "painter_color")
  private val rectTransformationMatrixLocation: WebGLUniformLocation =
    gl.getUniformLocation(rectShaderProgram, "u_transformation_matrix")
  private val rectCustomColorLocation: WebGLUniformLocation = gl.getUniformLocation(rectShaderProgram, "u_color")
  private val uRectangleSpecLocation = gl.getUniformLocation(rectShaderProgram, "u_rectangle_spec")

  private val rectCoordinates = new Float32Array(List(0,0,0, 1,0,0, 0,-1,0, 0,-1,0, 1,0,0, 1,-1,0).toJSArray)

  useProgram(rectShaderProgram)

  private val rectVertexBuffer: WebGLBuffer = gl.createBuffer()

  def drawRectangle(z: Complex, width: Double, height: Double, color: Vec4 = Vec4(1.0, 1.0, 1.0, 1.0),
                    lineWidth: Int = 0): Unit = {
    useProgram(rectShaderProgram)
    gl.bindBuffer(WebGLRenderingContext.ARRAY_BUFFER, rectVertexBuffer)
    gl.bufferData(WebGLRenderingContext.ARRAY_BUFFER, rectCoordinates, WebGLRenderingContext.STATIC_DRAW)
    gl.vertexAttribPointer(rectCoordLocation, 3, WebGLRenderingContext.FLOAT, normalized = false, 0, 0)
    gl.enableVertexAttribArray(rectCoordLocation)
    gl.bindBuffer(WebGLRenderingContext.ARRAY_BUFFER, null)
    gl.uniform4fv(rectCustomColorLocation, new Float32Array(color.toList.toJSArray))
    gl.uniformMatrix4fv(uRectangleSpecLocation, transpose = false, new Float32Array(
      (Matrix.translation3d(z.re, z.im, 0) * Matrix.scaling3d(width, height, 1)).array.toJSArray
    ))
    gl.uniform4fv(rectPaintingColorLocation, new Float32Array(drawingColor.toList.toJSArray))
    gl.drawArrays(WebGLRenderingContext.TRIANGLES, 0, 6)
  }

  def drawDisk(center: Complex, radius: Double, color: Vec4 = Vec4(1,1,1,1),
               segments: Int = 20, lineWidth: Int = 0): Unit = {
    val vertices = (0 until segments).flatMap(j => List(
      center,
      center + radius * Complex.exp(i * 2 * math.Pi * j / segments),
      center + radius * Complex.exp(i * 2 * math.Pi * (j + 1) / segments)
    ))
    drawVertices(vertices.toList, color, lineWidth)
  }

  def drawEllipse(center: Complex, xRadius: Double, yRadius: Double, rotation: Double = 0, color: Vec4 = Vec4(1,1,1,1),
                  segments: Int = 20, lineWidth: Int = 0): Unit = {
    val vertices = (for (j <- 0 to segments) yield
      center + (xRadius * math.cos(j * 2 * math.Pi / segments) + Complex.i * (
        yRadius * math.sin(j * 2 * math.Pi / segments)
        )) * Complex.rotation(rotation)).toVector
    drawVertices(vertices, color, lineWidth)
  }

  def drawLine(vertices: Seq[Complex], color: Vec4 = Vec4(1,1,1,1), lineWidth: Int = 2): Unit = ???


  def drawVertices(vertices: Seq[Complex], color: Vec4, lineWidth: Int = 0): Unit = {


    // we use the program created to draw vertices
    // Use the combined shader program object
    useProgram(shaderProgram)

    /*======== Defining and storing the geometry ===========*/


    // Bind appropriate array buffer to it
    gl.bindBuffer(WebGLRenderingContext.ARRAY_BUFFER, vertexBuffer)
    // Pass the vertex data to the buffer
    gl.bufferData(WebGLRenderingContext.ARRAY_BUFFER,
      new Float32Array(vertices.flatMap(z => List(z.re, z.im, 0.0)).toJSArray), WebGLRenderingContext.STATIC_DRAW)
    // Unbind the buffer
    //    gl.bindBuffer(WebGLRenderingContext.ARRAY_BUFFER, null)



    /*======= Associating shaders to buffer objects =======*/

    // Bind vertex buffer object
    //    gl.bindBuffer(WebGLRenderingContext.ARRAY_BUFFER, vertexBuffer)

    // Point an attribute to the currently bound VBO
    gl.vertexAttribPointer(coordLocation, 3, WebGLRenderingContext.FLOAT, normalized = false, 0, 0)
    // Enable the attribute
    gl.enableVertexAttribArray(coordLocation)



    /*=========Drawing the triangle===========*/
    // setting custom color
    gl.uniform4fv(customColorLocation, new Float32Array(color.toList.toJSArray))

    // setting the painting_color
    gl.uniform4fv(paintingColorLocation, new Float32Array(drawingColor.toList.toJSArray))


    // get the transformation_matrix location
    //      gl.uniformMatrix4fv(
    //        transformationMatrixLocation, transpose = false,
    //        new Float32Array(
    //          (Matrix.projection3d(canvas.width, canvas.height, canvas.width) * _transformationMatrix)
    //            .array.toJSArray)
    //      )


    if (lineWidth == 0)
    // Draw the triangle
      gl.drawArrays(WebGLRenderingContext.TRIANGLES, 0, vertices.length)
    else {
      gl.lineWidth(lineWidth)
      gl.drawArrays(WebGLRenderingContext.LINE_LOOP, 0, vertices.length)
    }
  }

  def drawTextureQuad(tex: html.Image, quad: Quad, topLeft: Complex, width: Double, height: Double): Unit = {
    // use program for texture
    useProgram(textureShaderProgram)

    // texture coordinate for the image rectangle
    gl.bindBuffer(WebGLRenderingContext.ARRAY_BUFFER, texCoordBuffer)
    gl.bufferData(
      WebGLRenderingContext.ARRAY_BUFFER,
      new Float32Array(List(
        quad.left / quad.imageWidth, quad.top / quad.imageHeight,
        quad.left / quad.imageWidth, quad.bottom / quad.imageHeight,
        quad.right / quad.imageWidth, quad.top / quad.imageHeight,
        quad.right / quad.imageWidth, quad.top / quad.imageHeight,
        quad.left / quad.imageWidth, quad.bottom / quad.imageHeight,
        quad.right / quad.imageWidth, quad.bottom / quad.imageHeight
      ).toJSArray),
      WebGLRenderingContext.STATIC_DRAW
    )

    // buffer for positions
    val vertices = List(
      topLeft.re, topLeft.im - height, 0,
      topLeft.re, topLeft.im, 0,
      topLeft.re + width, topLeft.im - height, 0,
      topLeft.re + width, topLeft.im - height, 0,
      topLeft.re, topLeft.im, 0,
      topLeft.re + width, topLeft.im, 0
    )
    gl.bindBuffer(WebGLRenderingContext.ARRAY_BUFFER, texturePositionBuffer)
    gl.bufferData(
      WebGLRenderingContext.ARRAY_BUFFER, new Float32Array(vertices.toJSArray), WebGLRenderingContext.STATIC_DRAW
    )



    gl.bindTexture(WebGLRenderingContext.TEXTURE_2D, webGLTexture)
    gl.texImage2D(
      WebGLRenderingContext.TEXTURE_2D, 0, WebGLRenderingContext.RGBA, WebGLRenderingContext.RGBA,
      WebGLRenderingContext.UNSIGNED_BYTE, tex
    )

    //gl.generateMipmap(WebGLRenderingContext.TEXTURE_2D)




    // vertices location
    gl.enableVertexAttribArray(positionLocation)
    gl.bindBuffer(WebGLRenderingContext.ARRAY_BUFFER, texturePositionBuffer)

    gl.vertexAttribPointer(
      positionLocation, 3, WebGLRenderingContext.FLOAT, normalized = false, 0, 0
    )


    // texcoord location
    gl.enableVertexAttribArray(texCoordLocation)
    gl.bindBuffer(WebGLRenderingContext.ARRAY_BUFFER, texCoordBuffer)

    gl.vertexAttribPointer(
      texCoordLocation, 2, WebGLRenderingContext.FLOAT, normalized = false, 0, 0
    )

    // set the painter color
    gl.uniform4fv(painterColorLocation, new Float32Array(drawingColor.toList.toJSArray))

    //    gl.uniform1i(textureLocation, 0)
    // this is to make sure we accept non power of two sided images
    gl.texParameteri(
      WebGLRenderingContext.TEXTURE_2D,
      WebGLRenderingContext.TEXTURE_MIN_FILTER,
      WebGLRenderingContext.LINEAR
    )
    gl.texParameteri(
      WebGLRenderingContext.TEXTURE_2D,
      WebGLRenderingContext.TEXTURE_WRAP_S,
      WebGLRenderingContext.CLAMP_TO_EDGE
    )
    gl.texParameteri(
      WebGLRenderingContext.TEXTURE_2D,
      WebGLRenderingContext.TEXTURE_WRAP_T,
      WebGLRenderingContext.CLAMP_TO_EDGE
    )

    gl.drawArrays(WebGLRenderingContext.TRIANGLES, 0, vertices.length / 3)


  }

  def drawImage(image: html.Image, topLeft: Complex, width: Double, height: Double): Unit =
    drawTexture(image, topLeft, width, height)

  def drawCanvas(canvas: html.Canvas, topLeft: Complex, width: Double, height: Double): Unit =
    drawTexture(canvas.asInstanceOf[HTMLImageElement], topLeft, width, height)

  def drawTexture(tex: HTMLImageElement, topLeft: Complex, width: Double, height: Double): Unit = {
    drawTextureQuad(tex, Quad(0,1,1,0,1,1), topLeft, width, height)
  }



  private val textCtx: CanvasRenderingContext2D = dom.document
    .createElement("canvas").asInstanceOf[HTMLCanvasElement]
    .getContext("2d").asInstanceOf[CanvasRenderingContext2D]
  private val textCanvas: HTMLCanvasElement = textCtx.canvas
  textCanvas.style.backgroundColor = "rgba(0,0,0,0)"

//  def print(text: String, z: Complex, width: Double, height: Double,
//            xOffset: Double, yOffset: Double,
//            font: String,
//            textAlign: String,
//            textBaseLine: String,
//            color: String,
//            alpha: Double): Unit = {
//
//    print(List((text, color)), z, width, height,
//      xOffset = xOffset, yOffset = yOffset,
//      font = font, textAlign = textAlign, textBaseLine = textBaseLine,
//      alpha = alpha)
//  }

  def print(texts: Seq[(String, String)], z: Complex, width: Double, height: Double,
            xOffset: Double = 0, yOffset: Double = 0,
            font: String = "20px monospace",
            textAlign: String = "left",
            textBaseLine: String = "middle",
            alpha: Double = 1.0): Unit = {

    textCtx.save()
    textCanvas.width = width.toInt
    textCanvas.height = height.toInt
    textCtx.font = font
    textCtx.textAlign = textAlign
    textCtx.textBaseline = textBaseLine
    textCtx.globalAlpha = alpha
    textCtx.clearRect(0, 0, width, height)

    var left: Double = 0
    texts.foreach({case (text, color) =>
      textCtx.fillStyle = color
      textCtx.fillText(text, left + xOffset, yOffset)
      left += textCtx.measureText(text).width
    })
    drawTexture(textCanvas.asInstanceOf[HTMLImageElement], z, width, height)
    textCtx.restore()
  }

  def textWidth(text: String, font: String): Double = {
    textCtx.save()
    textCtx.font = font
    val w = textCtx.measureText(text).width
    textCtx.restore()
    w
  }
  def textWidth(c: Char, font: String): Double = {
    textWidth(c.toString, font)
  }



  def clear(): Unit = {
//    setColor()
    //   setScissor()

    gl.enable(WebGLRenderingContext.BLEND)
    gl.blendFunc(WebGLRenderingContext.SRC_ALPHA, WebGLRenderingContext.ONE_MINUS_SRC_ALPHA)
    gl.viewport(0,0,canvas.width,canvas.height)
    gl.clearColor(backgroundColor.x1, backgroundColor.x2, backgroundColor.x3, 1.0)
    //gl.enable(WebGLRenderingContext.DEPTH_TEST)
    // Clear the color buffer bit
    gl.clear(WebGLRenderingContext.COLOR_BUFFER_BIT)
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////
  /// Transformation matrix manipulations ///
  ///////////////////////////////////////////

  def resetTransformationMatrix(): Unit = {
    _transformationMatrix = eye4
    matrixChanged()
  }

  def rotate(angle: Double, dim: Int = 3): Unit = {
    if (dim == 1)
      _transformationMatrix = Matrix.xRotation3d(angle) * _transformationMatrix
    else if (dim == 2) _transformationMatrix =  Matrix.yRotation3d(angle) * _transformationMatrix
    else _transformationMatrix = Matrix.zRotation3d(angle) * _transformationMatrix
    matrixChanged()
  }

  def scale(sx: Double, sy: Double, sz: Double = 1): Unit = {
    _transformationMatrix = Matrix.scaling3d(sx, sy, sz) * _transformationMatrix
    matrixChanged()
  }

  def translate(dx: Double, dy: Double, dz: Double = 0): Unit = {
    _transformationMatrix = Matrix.translation3d(dx, dy, dz) * _transformationMatrix
    matrixChanged()
  }

  def storeTransformationMatrix(): Unit =
    _storedTransformationMatrix = _transformationMatrix

  def restoreTransformationMatrix(): Unit = {
    _transformationMatrix = _storedTransformationMatrix
    matrixChanged()
  }

  def transformationMatrix: Matrix = _transformationMatrix.copy

  private def matrixChanged(): Unit = {
    useProgram(shaderProgram)
    gl.uniformMatrix4fv(
      transformationMatrixLocation, transpose = false,
      new Float32Array(
        (Matrix.projection3d(canvas.width, canvas.height, canvas.width) * _transformationMatrix)
          .array.toJSArray)
    )
    useProgram(textureShaderProgram)
    gl.uniformMatrix4fv(
      textureTransformationMatrixLocation, transpose = false,
      new Float32Array(
        (Matrix.projection3d(canvas.width, canvas.height, canvas.width) * _transformationMatrix)
          .array.toJSArray)
    )
    useProgram(rectShaderProgram)
    gl.uniformMatrix4fv(
      rectTransformationMatrixLocation, transpose = false,
      new Float32Array(
        (Matrix.projection3d(canvas.width, canvas.height, canvas.width) * _transformationMatrix)
          .array.toJSArray)
    )
  }
  matrixChanged()


}
