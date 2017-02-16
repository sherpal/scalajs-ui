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
import org.scalajs.dom.raw.{CanvasRenderingContext2D, HTMLCanvasElement}


class FontString(n: String, parent: Frame) extends LayeredRegion with FontInstance with Textable {
  val name: String = UIObject.setName(n)

  protected var _parent: Option[Frame] = None
  setParent(parent)

  setDrawLayer(drawLayer)

  private var _text: String = ""

  /** Returns the raw text associated to the FontString. */
  def text: String = _text

  /** Sets the raw text associated to the FontString. */
  def setText(text: String): Unit = {
    _text = text

    _textHasChanged = true
  }

  private var _password: Boolean = false

  /**
   * Returns whether FontString is in password mode.
   * A FontString in password mode will be shown as a sequence of * characters.
   */
  def isPassword: Boolean = _password

  /** Sets the password mode. */
  def setPassword(enable: Boolean = false): Unit = _password = enable


  private var _multiLine: Boolean = false

  /** Returns whether FontString can be written in several lines. If yes, it is also directly cut if too long. */
  def canMultiLine: Boolean = _multiLine

  /** Sets the multi line mode. */
  def setMultiLine(enable: Boolean = false): Unit = {
    _textHasChanged = true
    _multiLine = enable
  }

  private var _lineSpace: Double = 2

  /** Returns the space, in pixels, between two consecutive lines of FontString. */
  def lineSpace: Double = _lineSpace

  /** Sets the space, in pixels, between two consecutive lines of FontString. */
  def setLineSpace(space: Double = 2): Unit = {
    _textHasChanged = true
    _lineSpace = space
  }

  private var _wordWrap: Boolean = false

  /** Returns whether words can be split at any character when put on several lines. */
  def canWordWrap: Boolean = _wordWrap

  /** Sets whether words can be split at any character. */
  def setWordWrap(enable: Boolean = false): Unit = {
    _textHasChanged = true
    _wordWrap = enable
  }

  private var _autoHeight: Boolean = false

  /** Returns whether FontString should auto adjust its height to fit the lines. */
  def autoHeight: Boolean = _autoHeight

  /** Sets whether FontString should auto adjust its height to fit the lines. */
  def setAutoHeight(enable: Boolean): Unit = {
    _textHasChanged = true
    _autoHeight = enable

    if (enable) setHeight(1500)
  }

  /** Returns the height taken by all the lines.*/
  def totalHeight(): Double = {
    makeDrawInfo()

    drawInfo.allLines.length * (fontSize + _lineSpace) - _lineSpace
  }

  /** Draw the FontString if it's not in password mode. */
  private def normalDraw(): Unit = {
    Engine.painter.setColor(1, 1, 1, getEffectiveAlpha)

    drawInfo.allLines.foreach((lineInfo: LineInfo) => {
      Engine.painter.print(
        lineInfo.texts, Complex(left, top),
        width, height, lineInfo.x, lineInfo.y, font
      )
    })
  }

  /** Draw the FontString if it is in password mode. */
  private def passwordDraw(): Unit = {
    val (xOffset, jH): (Double, String) = justifyH match {
      case JustifyLeft => (0, "left")
      case JustifyCenter => (width / 2, "center")
      case JustifyRight => (width, "right")
    }

    Engine.painter.setColor(_red, _green, _blue, getEffectiveAlpha)
    Engine.painter.print("*" * _text.length, Complex(left, center._2), width, height, xOffset, 0,
      font, jH, baseline, "rgb(1,1,1)", 1)
  }

  /** Draws the FontString, according to its mode and possible [[ScrollFrame]] ancestor. */
  def draw(): Unit = if (isVisible) {
    makeDrawInfo()

    _scrollFrameAncestor match {
      case Some(scrollFrame) if scrollFrame.amIVisible(this) =>
        Engine.painter.setScissor(scrollFrame.left.toInt, scrollFrame.bottom.toInt,
          scrollFrame.width.toInt, scrollFrame.height.toInt)
        if (_password) passwordDraw() else normalDraw()
      case _ =>
        if (_password) passwordDraw() else normalDraw()
    }

    Engine.painter.setScissor()
  }



  /// helper functions

  private def splitInLines(coloredText: List[List[(String, String)]]): List[List[(String, String)]] = {
    if (!canMultiLine) {
      List(coloredText.flatten) // removing all \n characters
    } else {
      val tooLongIndex = coloredText.indexWhere(l => {
        l.map((elem: (String, String)) => FontString.textWidth(elem._1, font)).sum > width
      })
      if (tooLongIndex == -1) {
        coloredText
      } else {
        val (fittingLines, others): (List[List[(String, String)]], List[List[(String, String)]]) =
          coloredText.splitAt(tooLongIndex)
        val tooLongLine = others.head
        val widths: List[Double] = tooLongLine.scanLeft(0: Double)((acc: Double, words: (String, String)) => {
          acc + FontString.textWidth(words._1, font)
        })
        val exceedingIndex = widths.indexWhere(_ > width)
        if (scala.scalajs.LinkingInfo.developmentMode) {
          assert(exceedingIndex > 0, "Did not find exceeding index, which is weird")
        }
        val startingWidth = widths(exceedingIndex - 1)
        val (fitting, exceeding) = tooLongLine.splitAt(exceedingIndex - 1)
        if (canWordWrap) {
          val atoms: Array[String] = exceeding.head._1.split("")
          val splitIndex = atoms.scanLeft(startingWidth)((acc, c) => acc + FontString.textWidth(c, font))
            .indexWhere(_ > width)
          if (scala.scalajs.LinkingInfo.developmentMode) {
            assert(splitIndex > 0, "Did not find split index, which is weird")
          }
          if (exceedingIndex == 1 && splitIndex == 1) {
            (fittingLines :+ others.head) ++ splitInLines(others.tail)
          } else {
            val (fit, notFit): (Array[String], Array[String]) = atoms.splitAt(splitIndex - 1)
            val newFitting: List[(String, String)] = fitting :+ ((fit.mkString(""), exceeding.head._2))
            val newExceeding: List[(String, String)] = (notFit.mkString(""), exceeding.head._2) +: exceeding.tail
            val newOthers: List[List[(String, String)]] = if (others.tail.isEmpty) List(newExceeding) else
              newExceeding +: others.tail//.head +: others.tail
            (fittingLines :+ newFitting) ++ splitInLines(newOthers)
          }
        } else {
          val spaceLength = FontString.textWidth(" ", font)
          val atoms: Array[String] = exceeding.head._1.split(" ")
          val splitIndex = atoms.scanLeft(startingWidth)((acc, w) => acc + FontString.textWidth(w, font) + spaceLength)
            .indexWhere(_ > width)
          if (scala.scalajs.LinkingInfo.developmentMode) {
            assert(splitIndex > 0, "Did not find split index, which is weird")
          }
          if (exceedingIndex == 1 && splitIndex == 1) {
            (fittingLines :+ others.head) ++ splitInLines(others.tail)
          } else {
            val (fit, notFit): (Array[String], Array[String]) = atoms.splitAt(splitIndex - 1)
            val newFitting: List[(String, String)] = fitting :+ ((fit.mkString(" ") + " ", exceeding.head._2))
            val newExceeding: List[(String, String)] = (notFit.mkString(" ") + " ", exceeding.head._2) +: exceeding.tail
            val newOthers: List[List[(String, String)]] = if (others.tail.isEmpty) List(newExceeding) else
              newExceeding +: others.tail//.head +: others.tail
            (fittingLines :+ newFitting) ++ splitInLines(newOthers)
            //coloredText
          }
        }
      }
    }
  }


  private def strXCoord(txt: String): Double = justifyH match {
    case JustifyLeft => 0
    case JustifyRight => math.floor(width - Engine.painter.textWidth(txt, font))
    case JustifyCenter => math.floor(width / 2 - Engine.painter.textWidth(txt, font) / 2)
  }


  private var drawInfo: DrawInfo = new DrawInfo(0, 0, List())


  private def makeDrawInfo(): Unit = {
    // we need to set the biggest height possible otherwise canvas will not draw everything
    //if (_autoHeight) setHeight(Engine.painter.dimensions._2)
    if ((canBeDrawn || (_autoHeight && width > 0)) && _textHasChanged) {
      _textHasChanged = false

      val coloredText = FontString.textToColoredText(_text, FontString.rgbToHex(_textR, _textG, _textB))

      val allLines = splitInLines(coloredText)
      val l = allLines.length
      val fontHeight = fontSize

      if (_autoHeight) setHeight(l * (fontHeight + _lineSpace) - _lineSpace)

      val ys = (justifyV match {
        case JustifyTop => fontHeight / 2
        case JustifyMiddle => height / 2 - l / 2 * (fontHeight + _lineSpace) +
          (if (l % 2 == 0) fontHeight / 2 else 0)
        case JustifyBottom => height - l * (fontHeight + _lineSpace)
      }) to height by fontHeight + _lineSpace

      val linesInfo = allLines.zip(ys).map({case (line, h) => new LineInfo(line, strXCoord(
        line.foldLeft("")((acc: String, elem: (String, String)) => acc + elem._1)), h)})

      drawInfo = new DrawInfo(left, top, linesInfo)
    }
  }
}


object FontString {
  private val colorMatch = """color{[0-9A-Z][0-9A-Z][0-9A-Z][0-9A-Z][0-9A-Z][0-9A-Z]:[\s\S]*?}""".r
  //private val beginColorMatch = """color{[0-9A-Z][0-9A-Z][0-9A-Z][0-9A-Z][0-9A-Z][0-9A-Z]:""".r
  private val helperTextCtx: CanvasRenderingContext2D = dom.document
    .createElement("canvas").asInstanceOf[HTMLCanvasElement]
    .getContext("2d").asInstanceOf[CanvasRenderingContext2D]
//  private val helperTextCanvas: HTMLCanvasElement = helperTextCtx.canvas

  private def textWidth(text: String, font: String): Double = {
    helperTextCtx.font = font
    helperTextCtx.measureText(text).width
  }

  private def hexToRGB(hex: String): (Int, Int, Int) = {
    (
      Integer.valueOf(hex.substring(0,2), 16),
      Integer.valueOf(hex.substring(2,4), 16),
      Integer.valueOf(hex.substring(4,6), 16)
    )
  }

  private def rgbToHex(r: Double, g: Double, b: Double): String = {
    val rHex = Integer.toHexString((r * 255).toInt)
    val gHex = Integer.toHexString((g * 255).toInt)
    val bHex = Integer.toHexString((b * 255).toInt)
    ((if (rHex.length == 1) 0 + rHex else rHex) +
      (if (gHex.length == 1) 0 + gHex else gHex) +
      (if (bHex.length == 1) 0 + bHex else bHex)).toUpperCase
  }

  private def fillReturnLineColor(s: String): String = colorMatch.replaceAllIn(s, m => {
    "\\n".r.replaceAllIn(m.toString(), _ => "}\n" + m.toString().substring(0, 13))
  })

  private def fillDefaultColors(s: String, defaultColor: String): String = {
    val coloredTexts: Array[String] = (for (t <- colorMatch.findAllIn(s)) yield t.toString).toArray
    val nonColoredTexts: Array[String] = s.split(colorMatch.toString())
      .map(v => if (v != "\n") "color{" + defaultColor + ":" + v + "}" else "\n")

    val temp = nonColoredTexts.zip(coloredTexts).map(elem => elem._1 + elem._2).mkString("")
    if (nonColoredTexts.length > coloredTexts.length) temp + nonColoredTexts.last else temp
  }

  private def substringToColoredText(s: String): (String, String) = {
    val (r, g, b) = hexToRGB(s.substring(6,12))
    (s.substring(13, s.length - 1), "rgb(" + r + "," + g + "," + b + ")")
  }

  private def textToColoredText(s: String, defaultColor: String): List[List[(String, String)]] =
    fillReturnLineColor(fillDefaultColors(s, defaultColor))
      .split("\n")
      .map(u => (for (v <- colorMatch.findAllIn(u)) yield {
        substringToColoredText(v)
      }).toList)
      .toList


}


class LineInfo(val texts: List[(String, String)], val x: Double, val y: Double)

class DrawInfo(val left: Double, val top: Double,
               val allLines: List[LineInfo])
