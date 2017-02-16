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


trait FontInstance extends UIObject {

  // encodes whether the text has changed and position have to be computed again.
  protected var _textHasChanged: Boolean = true

  private var _font: String = "Quicksand"
  private val defaultFontSize = 20
  private var _size: Int = defaultFontSize

  /** Returns the size font and font name in a [[String]] format that is suitable for canvases draws. */
  def font: String = _size + "px " + _font

  /** Sets the font family and size. Size defaults to 20. */
  def setFont(family: String, size: Int = defaultFontSize): Unit = {
    _textHasChanged = true
    _font = family
    _size = size
  }

  /** Returns the font size as an [[Int]]. */
  def fontSize: Int = _size

  /** Sets the size of the font. */
  def setFontSize(size: Int): Unit = {
    _textHasChanged = true
    _size = size
  }

  /** Returns a couple font family and font size. */
  def fontProperties: (String, Int) = (_font, _size)


  private var _justifyH: JustifyH = JustifyCenter

  /** Returns the Horizontal alignment of the font. */
  def justifyH: JustifyH = _justifyH

  /** Sets the Horizontal alignment of the font. */
  def setJustifyH(justify: JustifyH = JustifyCenter): Unit = {
    _textHasChanged = true
    _justifyH = justify
  }

  private var _justifyV: JustifyV = JustifyMiddle

  /** Returns the Vertical alignment of the font. */
  def justifyV: JustifyV = _justifyV

  /** Sets the vertical alignment of the font. */
  def setJustifyV(justify: JustifyV = JustifyMiddle): Unit = {
    _textHasChanged = true
    _justifyV = justify
  }

  private var _baseline: BaseLine = BaseLineAlphabetic
  def baseline: String = _baseline.canvas2dName
  def setBaseline(baseline: BaseLine = BaseLineAlphabetic): Unit = {
    _textHasChanged = true
    _baseline = baseline
  }

  protected var _textR: Double = 1.0
  protected var _textG: Double = 1.0
  protected var _textB: Double = 1.0
  protected var _textAlpha: Double = 1.0

  /** Sets the text colours and alpha. */
  def setTextColor(r: Double = 1.0, g: Double = 1.0, b: Double = 1.0, alpha: Double = 1.0): Unit = {
    _textAlpha = alpha
    _textR = r
    _textG = g
    _textB = b

    _textHasChanged = true
  }

  /** Returns the text colours and alpha. */
  def textColor: (Double, Double, Double, Double) = (_textR, _textG, _textB, _textAlpha)

}



abstract sealed class Justify {
  def canvas2dName: String
}

abstract sealed class JustifyH extends Justify
case object JustifyLeft extends JustifyH {def canvas2dName: String = "left"}
case object JustifyCenter extends JustifyH {def canvas2dName: String = "center"}
case object JustifyRight extends JustifyH {def canvas2dName: String = "right"}


abstract sealed class JustifyV extends Justify
case object JustifyTop extends JustifyV {def canvas2dName: String = "top"}
case object JustifyMiddle extends JustifyV {def canvas2dName: String = "middle"}
case object JustifyBottom extends JustifyV {def canvas2dName: String = "bottom"}

// Reusing vertical baseline of CanvasRendering2D
// Baseline alignment setting. Possible values:
// top, hanging, middle, alphabetic, ideographic, bottom.
// The default value is alphabetic.
abstract sealed class BaseLine extends Justify
case object BaseLineTop extends BaseLine {def canvas2dName: String = "top"}
case object BaseLineHanging extends BaseLine {def canvas2dName: String = "hanging"}
case object BaseLineMiddle extends BaseLine {def canvas2dName: String = "middle"}
case object BaseLineAlphabetic extends BaseLine {def canvas2dName: String = "alphabetic"}
case object BaseLineIdeographic extends BaseLine {def canvas2dName: String = "ideographic"}
case object BaseLineBottom extends BaseLine {def canvas2dName: String = "bottom"}

