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


trait LayeredRegion extends Region {

  def draw(): Unit

  private var _drawLayer: Layer = Artwork
  private var _drawSubLayer: Int = 0

  /** Returns the draw layer of the Region. */
  def drawLayer: Layer = _drawLayer

  /** Returns the draw sub layer of the Region. */
  def drawSubLayer: Int = _drawSubLayer

  /** Sets the draw layer and sublayer of the Region. */
  def setDrawLayer(layer: Layer, subLayer: Int): Unit = {
    _drawLayer = layer
    _drawSubLayer = subLayer

    parent match {
      case Some(p) => p.orderLayeredRegions()
      case _ =>
        if (scala.scalajs.LinkingInfo.developmentMode) {
          println("Something weird, LayeredRegion does not have parent...")
        }
    }
  }

  /** Sets the layer of the region, and the sublayer to 9 if Region is a FontString, 0 otherwise. */
  def setDrawLayer(layer: Layer): Unit = setDrawLayer(layer, this match {
    case _: FontString => 9
    case _ => 0
  })

  protected var _red: Double = 1.0
  protected var _green: Double = 1.0
  protected var _blue: Double = 1.0

  /** Sets the colours and the alpha of the Region. */
  def setVertexColor(red: Double = 1.0, green: Double = 1.0, blue: Double = 1.0, alpha: Double = 1.0): Unit = {
    _red = if (red < 0) 0.0 else if (red > 1) 1.0 else red
    _green = if (green < 0) 0.0 else if (green > 1) 1.0 else green
    _blue = if (blue < 0) 0.0 else if (blue > 1) 1.0 else blue
    setAlpha(alpha)
  }
}


