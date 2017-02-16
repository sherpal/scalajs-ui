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

package Matrix



class Matrix(val m: Int, val n: Int) {
  private val matrixArray: Array[Double] = new Array[Double](m * n)

  def apply(i: Int, j: Int): Double = matrixArray(i + j * n)
  def apply(i: Int): Double = matrixArray(i)

  def update(i: Int, j: Int, a: Double): Unit = matrixArray(i + j * n) = a

  def copy: Matrix = {
    val c = new Matrix(m, n)
    for {
      i <- 0 until m
      j <- 0 until n
    } {
      c(i, j) = this(i, j)
    }
    c
  }

  def array: Array[Double] = matrixArray.clone

  def *(that: Matrix): Matrix = {
    assert(this.n == that.m, s"size mismatch for matrix multiplication, left operand is ${this.m}x${this.n} " +
    s"right operand is ${that.m}x${that.n}")
    val mult = new Matrix(this.m, that.n)

    for {
      i <- 0 until this.m
      j <- 0 until that.n
    } {
      var a = 0.0
      (0 until this.n).foreach(k => a += this(i, k) * that(k, j))
      mult(i, j) = a
    }

    mult
  }


  def sum: Double = matrixArray.sum

}


object Matrix {
  def apply(m: Int, n: Int): Matrix = new Matrix(m, n)

  def eye(n: Int): Matrix = {
    val e = Matrix(n, n)
    for {
      i <- 0 until n
      j <- 0 until n
    } {
      e(i, j) = if (i == j ) 1 else 0
    }
    e
  }

  def zeros(n: Int): Matrix = {
    val z = Matrix(n, n)
    for {
      i <- 0 until n
      j <- 0 until n
    } {
      z(i, j) = 0
    }
    z
  }

  def translation3d(dx: Double, dy: Double, dz: Double): Matrix = {
    val translation = eye(4)
    translation(0,3) = dx
    translation(1,3) = dy
    translation(2,3) = dz
    translation
  }

  def xRotation3d(angle: Double): Matrix = {
    val xRot = eye(4)
    val cos = math.cos(angle)
    val sin = math.sin(angle)
    xRot(1,1) = cos
    xRot(1,2) = -sin
    xRot(2,1) = sin
    xRot(2,2) = cos
    xRot
  }

  def yRotation3d(angle: Double): Matrix = {
    val yRot = eye(4)
    val cos = math.cos(angle)
    val sin = math.sin(angle)
    yRot(0,0) = cos
    yRot(0,2) = -sin
    yRot(2,0) = sin
    yRot(2,2) = cos
    yRot
  }

  def zRotation3d(angle: Double): Matrix = {
    val zRot = eye(4)
    val cos = math.cos(angle)
    val sin = math.sin(angle)
    zRot(0,0) = cos
    zRot(0,1) = -sin
    zRot(1,0) = sin
    zRot(1,1) = cos
    zRot
  }

  def scaling3d(sx: Double, sy: Double, sz: Double): Matrix = {
    val scale = eye(4)
    scale(0,0) = sx
    scale(1,1) = sy
    scale(2,2) = sz
    scale
  }

  // convert coordinates in screen space to webgl space, i.e. in (-1,1)
  def projection3d(width: Double, height: Double, depth: Double): Matrix = scaling3d(2/width, 2/height, 2/depth)
}