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

import gameengine.Engine

import scala.collection.mutable

import scala.scalajs.js.timers._


class Tooltip(n: String, par: Option[Frame]) extends Frame(n, par) {

  def this(parent: Frame) = this("", Some(parent))
  def this() = this("", Some(UIParent))


  private var lines: List[(FontString, Option[FontString])] = List()

  /**
   * Adds a [[FontString]] line to the Tooltip, at the bottom of the last one.
   *
   * @param text    text to put on the line.
   * @param red     red color for the text, between 0 and 1.
   * @param green   green color for the text, between 0 and 1.
   * @param blue    blue color for the text, between 0 and 1.
   * @param wrap    tells whether the FontString may be wrap into several lines.
   * @param justify horizontal alignment of the FontString.
   * @param size    font size
   * @return        the [[FontString]] used to draw the line. Can be newly created or reused FontString.
   */
  def addLine(text: String, red: Double, green: Double, blue: Double, wrap: Boolean = false,
              justify: JustifyH = JustifyLeft, size: Int = 12): FontString = {
    val fs = usableFontString()
    fs.show()
    fs.clearAllPoints()

    if (lines.isEmpty) {
      fs.setPoint(AnchorPoint(TopLeft, this, TopLeft))
      fs.setPoint(AnchorPoint(TopRight, this, TopRight))
    } else {
      fs.setPoint(AnchorPoint(TopLeft, lines.last._1, BottomLeft))
      fs.setPoint(AnchorPoint(TopRight, lines.last._1, BottomRight))
    }

    fs.setHeight(size)
    fs.setAutoHeight(enable = true)

    fs.setMultiLine(enable = wrap)
    fs.setText(text)
    fs.setFontSize(size)
    fs.setTextColor(red, green, blue)

    fs.setHeight(fs.totalHeight())
    fs.setJustifyH(justify)

    lines = lines :+ (fs, None)

    if (_autoAdjustHeight) adjustHeight()

    fs
  }

  /**
   * Adds a line consisting of two [[FontString]]s, one at the left and one at the right.
   *
   * @param textLeft   text content for the left FontString.
   * @param textRight  text content for the right FontString.
   * @param redLeft    red color for the left FontString.
   * @param greenLeft  green color for the left FontString.
   * @param blueLeft   blue color for the left FontString.
   * @param sizeLeft   font size for the left FontString.
   * @param redRight   red color for the right FontString.
   * @param greenRight green color for the right FontString.
   * @param blueRight  blue color for the right FontString.
   * @param sizeRight  font size for the right FontString.
   */
  def addDoubleLine(textLeft: String, textRight: String,
                    redLeft: Double, greenLeft: Double, blueLeft: Double, sizeLeft: Int,
                    redRight: Double, greenRight: Double, blueRight: Double, sizeRight: Int): Unit = {
    val left = addLine(textLeft, redLeft, greenLeft, blueLeft, size = sizeLeft)

    val fs = usableFontString()
    fs.show()
    fs.clearAllPoints()
    fs.setPoint(AnchorPoint(Left, left, Left))
    fs.setPoint(AnchorPoint(Right, left, Right))
    fs.setText(textRight)
    fs.setJustifyH(JustifyRight)
    fs.setHeight(sizeRight)
    fs.setHeight(fs.totalHeight())
    fs.setTextColor(redRight, greenRight, blueRight)
    fs.setFontSize(sizeRight)

    lines = lines.dropRight(1) :+ (left, Some(fs))

    if (_autoAdjustHeight) adjustHeight()
  }


  private var _autoAdjustHeight: Boolean = false

  /** Adjusts the height of the Tooltip to match the total height of all its lines. */
  def adjustHeight(): Unit = {
    setHeight(lines.map(line => math.max(line._1.height, line._2 match {
      case Some(fs) => fs.height
      case None => 0
    })).sum)
  }

  /** Returns whether Tooltip should auto adjust its height. */
  def autoAdjustHeight: Boolean = _autoAdjustHeight

  /** Sets whether Tooltip should auto adjust its height. */
  def setAutoAdjustHeight(enable: Boolean = false): Unit = {
    _autoAdjustHeight = enable

    if (enable) adjustHeight()
  }


  /**
   * Makes the Tooltip appear by increasing its alpha over time.
   *
   * @param appearTime duration of the animation, in milliseconds. Defaults to 500 ms.
   */
  def appear(appearTime: Int = 500): Unit = {
    Tooltip.addAppearingTooltip(this, appearTime)
  }

  /**
   * Calls the appear method after delay ms, with an appearing time of appearTime ms.
   * This can be used to make a Tooltip after the mouse has spent some time on an object.
   */
  private var appearTimeoutHandle: Option[SetTimeoutHandle] = None
  def appearIn(delay: Int = 500, appearTime: Int = 500): Unit = {
    appearTimeoutHandle = Some(setTimeout(delay) {
      appear(appearTime)
    })
  }

  /**
   * Makes the Tooltip to decreasing its alpha over time.
   *
   * @param fadingTime duration of the fading out animation, in milliseconds. Default to 500.
   */
  def fadeOut(fadingTime: Int = 500): Unit = {
    appearTimeoutHandle match {
      case Some(handle) => clearTimeout(handle)
      case _ =>
    }
    Tooltip.addFadingOutTooltip(this, fadingTime)
  }

  /** Removes the idx-th line of the Tooltip. */
  def clearLine(idx: Int): Unit = if (lines.nonEmpty) {
    if (scala.scalajs.LinkingInfo.developmentMode) {
      assert(idx >= 0 && idx < lines.length, "trying to clear non existing line")
    }

    val length = lines.length

    val lineIdx = math.min(length, idx)

    val line = lines(lineIdx)

    line._2 match {
      case Some(fontString) => addGarbageFS(fontString)
      case _ =>
    }
    addGarbageFS(line._1)

    if (length > lineIdx + 1) {
      val nextFS = lines(lineIdx + 1)._1
      nextFS.clearAllPoints()
      if (lineIdx == 0) {
        nextFS.setPoint(AnchorPoint(TopLeft, this, TopLeft))
        nextFS.setPoint(AnchorPoint(TopRight, this, TopRight))
      } else {
        val prevFS = lines(lineIdx - 1)._1
        nextFS.setPoint(AnchorPoint(TopLeft, prevFS, BottomLeft))
        nextFS.setPoint(AnchorPoint(TopRight, prevFS, BottomRight))
      }
    }

    lines = lines.filter(_ != line)

    if (_autoAdjustHeight) adjustHeight()
  }

  /** Removes the last line of the Tooltip. */
  def clearLine(): Unit = clearLine(lines.length - 1)

  /** Removes all lines of the Tooltip. */
  def clearLines(): Unit = {
    for (j <- lines.indices.reverse) clearLine(j)
  }

  private var _anchorType: TooltipAnchor = TooltipNone
  private var _anchorXOffset: Double = 0
  private var _anchorYOffset: Double = 0

  /** Returns Tooltip anchor type and offsets. */
  def anchorType: (TooltipAnchor, Double, Double) = (_anchorType, _anchorXOffset, _anchorYOffset)

  /**
   * Sets the [[TooltipAnchor]] of the Tooltip.
   *
   * @param anchorType anchor type to use. Options are:
   *                   - None, which makes the Tooltip to stay where he is,
   *                   - Cursor, the BottomRight point of the Tooltip is attached to the mouse cursor, or
   *                   - Any [[Point]], and in that case it is attached to the opposite point of its owner Frame.
   * @param xOffset    amount of pixels the anchor has to be shifted to the right.
   * @param yOffset    amount of pixels the anchor has to be shifted to the top.
   */
  def setAnchorType(anchorType: TooltipAnchor = TooltipNone, xOffset: Double = 0, yOffset: Double = 0): Unit = {
    Tooltip.removeCursorAnchoredTooltip(this)

    _anchorType = anchorType
    _anchorXOffset = xOffset
    _anchorYOffset = yOffset

    if (_anchorType.anchorToOwner.isDefined) {
      clearAllPoints()
      // we need to set a (possibly) temporary position in order to check the clamp frame position
      setPoint(AnchorPoint(_anchorType.anchorToOwner.get, _owner, _anchorType.asPoint.get,
        _anchorXOffset, _anchorYOffset))
      val (addXOffset, addYOffset): (Double, Double) = clamp match {
        case None => (0, 0)
        case Some(frame) =>
          (
            if (-math.min(0, left - frame.left) > 0) -math.min(0, left - frame.left)
            else - math.max(0, right - frame.right),
            if (- math.min(0, bottom - frame.bottom) > 0) - math.min(0, bottom - frame.bottom)
            else - math.max(0, top - frame.top)
          )
      }
      if (addXOffset != 0 || addYOffset != 0) {
        clearAllPoints()
        setPoint(AnchorPoint(_anchorType.anchorToOwner.get, _owner, _anchorType.asPoint.get,
          _anchorXOffset + addXOffset, _anchorYOffset + addYOffset))
      }
    } else if (_anchorType == TooltipCursor) {
      clearAllPoints()
      val (x, y) = Engine.mousePosition
      setPoint(AnchorPoint(BottomRight, UIParent, TopLeft, x + _anchorXOffset, y + _anchorYOffset))
      Tooltip.addCursorAnchoredTooltip(this, _anchorXOffset, _anchorYOffset)
    }
  }


  private var _owner: Frame = UIParent

  /** Returns the owner of the Tooltip. */
  def owner: Frame = _owner

  /**
   * Sets the owner of the Tooltip.
   * This can be used to easily have access to the owner's information.
   */
  def setOwner(owner: Frame): Unit = {
    _owner = owner

    if (_anchorType.anchorToOwner.isDefined) setAnchorType(_anchorType, _anchorXOffset, _anchorYOffset)
  }


  private val garbageFontStrings: mutable.Queue[FontString] = mutable.Queue()
  private def addGarbageFS(fs: FontString): Unit = {
    fs.clearAllPoints()
    fs.hide()
    garbageFontStrings.enqueue(fs)
  }

  private def usableFontString(): FontString =
    if (garbageFontStrings.isEmpty) createFontString()
    else garbageFontStrings.dequeue()

  setFrameStrata(TooltipStratum)
}


object Tooltip {

  private class TooltipInfo(val tooltip: Tooltip, var alpha: Double, val time: Int) {
    override def equals(that: Any): Boolean = that match {
      case that: TooltipInfo => this.tooltip == that.tooltip
      case _ => false
    }

    override def hashCode(): Int = tooltip.hashCode()
  }

  private val appearingTooltips: mutable.Set[TooltipInfo] = mutable.Set()
  private val fadingOutTooltips: mutable.Set[TooltipInfo] = mutable.Set()
  private val cursorAnchored: mutable.Set[(Tooltip, Double, Double)] = mutable.Set()

  private def addCursorAnchoredTooltip(tooltip: Tooltip, xOffset: Double, yOffset: Double): Unit = {
    cursorAnchored += ((tooltip, xOffset, yOffset))
  }

  private def removeCursorAnchoredTooltip(tooltip: Tooltip): Unit = {
    cursorAnchored -= ((tooltip, tooltip._anchorXOffset, tooltip._anchorYOffset))
  }

  private def addAppearingTooltip(tooltip: Tooltip, appearingTime: Int): Unit = {
    val startAlpha: Double = if (tooltip.isVisible) tooltip.alpha * appearingTime
    else 0

    tooltip.show()

    appearingTooltips += new TooltipInfo(tooltip, startAlpha, appearingTime)

    removeFadingOutTooltip(tooltip)

  }

  private def addFadingOutTooltip(tooltip: Tooltip, fadingTime: Int): Unit = {
    fadingOutTooltips += new TooltipInfo(tooltip, tooltip.alpha * fadingTime, fadingTime)

    removeAppearingTooltip(tooltip)
  }

  private def removeAppearingTooltip(tooltip: Tooltip): Unit = {
    appearingTooltips -= new TooltipInfo(tooltip, 0, 0)
  }

  private def removeFadingOutTooltip(tooltip: Tooltip): Unit = {
    fadingOutTooltips -= new TooltipInfo(tooltip, 0, 0)
  }

  /**
   * Makes all the appearing Tooltips to appear,
   * all the fading out Tooltips to fade out,
   * and move cursor anchored Tooltips.
   */
  def updateTooltips(dt: Double): Unit = {
    (for (info <- fadingOutTooltips) yield {
      info.alpha = info.alpha - dt
      if (info.alpha < 0) {
        info.tooltip.hide()
        (info.tooltip, true)
      } else {
        info.tooltip.setAlpha(info.alpha / info.time)
        (info.tooltip, false)
      }
    }).filter(elem => elem._2).foreach(elem => removeFadingOutTooltip(elem._1))

    (for (info <- appearingTooltips) yield {
      info.alpha = info.alpha + dt
      if (info.alpha > info.time) {
        info.tooltip.setAlpha(1)
        (info.tooltip, true)
      } else {
        info.tooltip.setAlpha(info.alpha / info.time)
        (info.tooltip, false)
      }
    }).filter(elem => elem._2).foreach(elem => removeAppearingTooltip(elem._1))

    cursorAnchored.foreach(elem => {
      val tooltip = elem._1
      val xOffset = elem._2
      val yOffset = elem._3
      val (x, y) = Engine.mousePosition

      if (tooltip.isVisible) {
        tooltip.clearAllPoints()
        tooltip.setPoint(AnchorPoint(BottomRight, UIParent, Center, x + xOffset, y + yOffset))
      }
    })
  }
}

abstract sealed class TooltipAnchor {
  val asPoint: Option[Point]
  val anchorToOwner: Option[Point]
}
case object TooltipBottom extends TooltipAnchor {
  val asPoint: Option[Point] = Some(Bottom)
  val anchorToOwner: Option[Point] = Some(Top)
}
case object TooltipBottomLeft extends TooltipAnchor {
  val asPoint: Option[Point] = Some(BottomLeft)
  val anchorToOwner: Option[Point] = Some(TopRight)
}
case object TooltipBottomRight extends TooltipAnchor {
  val asPoint: Option[Point] = Some(BottomRight)
  val anchorToOwner: Option[Point] = Some(TopRight)
}
case object TooltipCursor extends TooltipAnchor {
  val asPoint: Option[Point] = None
  val anchorToOwner: Option[Point] = None
}
case object TooltipLeft extends TooltipAnchor {
  val asPoint: Option[Point] = Some(Left)
  val anchorToOwner: Option[Point] = Some(Right)
}
case object TooltipRight extends TooltipAnchor {
  val asPoint: Option[Point] = Some(Right)
  val anchorToOwner: Option[Point] = Some(Left)
}
case object TooltipNone extends TooltipAnchor {
  val asPoint: Option[Point] = None
  val anchorToOwner: Option[Point] = None
}
case object TooltipTop extends TooltipAnchor {
  val asPoint: Option[Point] = Some(Top)
  val anchorToOwner: Option[Point] = Some(Bottom)
}
case object TooltipTopLeft extends TooltipAnchor {
  val asPoint: Option[Point] = Some(TopLeft)
  val anchorToOwner: Option[Point] = Some(BottomRight)
}
case object TooltipTopRight extends TooltipAnchor {
  val asPoint: Option[Point] = Some(TopRight)
  val anchorToOwner: Option[Point] = Some(BottomLeft)
}

