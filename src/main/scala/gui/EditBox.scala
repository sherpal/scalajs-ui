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
import scala.util.matching.Regex


class EditBox(n: String = "", parent: Option[Frame] = Some(UIParent)) extends Frame(n, parent)
  with Focusable with Textable {

  def this(parent: Frame) = this("", Some(parent))
  def this() = this("", Some(UIParent))


  /// Managing attached FontString
  private val fontString: FontString = createFontString()
  fontString.setAllPoints()
  fontString.setJustifyH(JustifyLeft)
  fontString.setJustifyV(JustifyMiddle)
  fontString.setDrawLayer(Overlay)

  /**
   * Returns the child [[FontString]] responsible of drawing the text.
   * This will be removed in the future, as we should be able to do everything we want from the EditBox.
   */
  def childFontString: FontString = fontString

  /** Sets the text colours and alpha. */
  def setTextColor(r: Double, g: Double, b: Double, alpha: Double = 1.0): Unit = {
    fontString.setTextColor(r, g, b)
    setBlinkFrame()
  }


  private val blinkFrame: Frame = new Frame("", Some(this))
  private val blinkTex: Texture = blinkFrame.createTexture()

  private def setBlinkFrame(): Unit = {
    blinkFrame.setWidth(2)
    blinkFrame.setHeight(fontString.fontSize - 1)

    blinkTex.setAllPoints()
    val (r, g, b, _) = fontString.textColor
    blinkTex.setVertexColor(r, g, b)

    var lastVisibleSwap: Double = 0

    blinkFrame.setScript(ScriptKind.OnUpdate)((_: ScriptObject, dt: Double) => {
      lastVisibleSwap += dt
      if (lastVisibleSwap > blinkSpeed) {
        lastVisibleSwap -= blinkSpeed
        if (blinkTex.isVisible) blinkTex.hide()
        else {
          blinkTex.show()
        }
      }
      putBlinkAtPosition()
    })
  }

  private def putBlinkAtPosition(): Unit = {
    blinkFrame.clearAllPoints()
    blinkFrame.setPoint(AnchorPoint(Left, fontString, Left,
      Engine.painter.textWidth(text.take(cursorPosition - 1), fontString.font)))
  }

  // TODO: setBlinkFrame each time a property could involve it to change. I think it's done, to be double checked...



  /// Managing blink
  private var _blinkSpeed: Int = 400 // unit of time: millisecond

  /** Returns the speed at which the blinkFrame appears and disappears. */
  def blinkSpeed: Int = _blinkSpeed

  /** Sets the blink speed of the blinkFrame, in milliseconds. */
  def setBlinkSpeed(speed: Int): Unit = _blinkSpeed = speed





  // TODO: highlight text and stuff

  /**
   * The history of an EditBox helps keeping track of previous commands entered by the user.
   * A typical use of HistoryLines would be having this code snippet inside an OnKeyPressed handler:
   * TODO: /!\ Copy-Paste from a previous implementation in Lua, needs to be changed.
   *
   * if key == 'up' then
   * local history = self:getHistoryLines()
   *
   * if #history == 0 then return nil end
   *
   * self.history_count = self.history_count or #history + 1
   * self.history_count = math.max(self.history_count - 1, 1)
   * self:setText(history[self.history_count])
   * self:setCursorPosition(string.len(self:getText()) + 1)
   * elseif key == 'down' then
   * if not self.history_count then return nil end
   *
   * local history = self:getHistoryLines()
   * self.history_count = self.history_count + 1
   *
   * if self.history_count > #history then
   *   self.history_count = nil
   * self:setText("")
   * self:setCursorPosition(1)
   * return nil
   * end
   *
   * self:setText(history[self.history_count])
   * self:setCursorPosition(string.len(self:getText()) + 1)
   * end
   *
   * The number of lines kept in the History can be set using setMaxHistoryLines
   * method. Default to 10.
   */
  private val _historyLines: mutable.Queue[String] = mutable.Queue()
  private var _maxHistoryLines: Int = 10

  /** Returns the maximum number of lines the EditBox stores. */
  def maxHistoryLines: Int = _maxHistoryLines

  /** Sets the maximum number of lines the EditBox stores. */
  def setMaxHistoryLines(max: Int = 10): Unit = {
    _maxHistoryLines = max
    // removing exceeding lines due to new maximum.
    while (_historyLines.length > _maxHistoryLines) _historyLines.dequeue()
  }

  /**
   * Returns a List containing each history line.
   * The head of the List is the last line that entered the history.
   */
  def historyLines: List[String] = _historyLines.toList.reverse

  /** Adds a new line of text to the history. Removing exceeding lines, if relevant. */
  def addHistoryLines(text: String): Unit = {
    _historyLines.enqueue(text)
    while (_historyLines.length > _maxHistoryLines) _historyLines.dequeue()
  }

  /** Removes every lines. */
  def clearHistory(): Unit = {
    _historyLines.clear()
  }




  /// Manage text


  /// Retrieving text info

  private var _maxLetterNumber: Int = 255

  /** Returns the maximum number of letters the EditBox accepts. */
  def maxLetterNumber: Int = _maxLetterNumber

  /** Sets the maximum number of letters the EditBox accepts. Removes exceeding characters, if relevant. */
  def setMaxLetterNumber(max: Int = 255): Unit = {
    _maxLetterNumber = max

    setText(text)
  }

  /** Returns the current number of letters. */
  def numLetters: Int = text.length

  private var numeric: Boolean = false

  /** Returns the numeric mode. */
  def isNumeric: Boolean = numeric

  /** Sets the numeric mode. */
  def setNumeric(enable: Boolean = false): Unit = {
    numeric = enable

    setText(text)
  }

  /** Returns the text of the EditBox as an [[Int]] if the the EditBox is numeric, false otherwise. */
  def number: Int = if (numeric) text.toInt else 0

  /// Deleting text

  /**
   * Removes the character just left to Cursor.
   * This behaviour corresponds to usual backspace behaviour.
   */
  def backspace(): Unit = {
    if (cursorPosition > 1) {
      setCursorPosition(cursorPosition - 1)
      val (before, after) = text.splitAt(cursorPosition - 1)
      setText(before + after.tail)
    }
  }

  /**
   * Deletes character just to the right of Cursor.
   * Corresponds to a usual behaviour when pressing the Delete key.
   */
  def delChar(): Unit = {
    val (before, after) = text.splitAt(cursorPosition - 1)
    setText(before + after.tail)
  }

  /// Adding text

  /**
   * Inserts the [[String]] s at the current position of the Cursor.
   * This does NOT remove highlighted text by default. If you want that kind of behaviour,
   * you have to first remove highlighted text.
   */
  def insert(s: String): Unit = {
    val (before, after) = text.splitAt(cursorPosition - 1)
    setText(before + s + after)
    setCursorPosition(cursorPosition + s.length)
  }

  private def isInt(s: String): Boolean = s.head == '-' && s.tail.forall(c => c.isDigit)

  /** Sets the specified text for the EditBox. */
  def setText(s: String): Unit = {
    // this needs to be adapted to more complicated type of numbers
    if (numeric) {
      if (s.isEmpty) setText(0)
      else if (isInt(s)) setText(s.toInt)
      else setText(0)
    } else {
      fontString.setText(s.take(_maxLetterNumber))
    }
    setCursorPosition(cursorPosition)
  }
  def setText(num: Int): Unit = {
    fontString.setText(num.toString.take(_maxLetterNumber))

    setCursorPosition(cursorPosition)
  }
  def setText(): Unit = {
    fontString.setText("")

    setCursorPosition(cursorPosition)
  }

  /** Returns the raw text of the EditBox. */
  def text: String = fontString.text




  /// Managing blink positions

  private var _cursorPosition: Int = 1
  /** Returns the current position of the cursor, between 1 and numLetters + 1. */
  def cursorPosition: Int = _cursorPosition

  /** Manually sets the position of the Cursor. */
  def setCursorPosition(pos: Int = 1): Unit = _cursorPosition = math.max(1, math.min(pos, numLetters + 1))

  /** Moves cursor -n to the left if n < 0, and n to the right if n > 0. */
  def moveCursor(n: Int): Unit = setCursorPosition(cursorPosition + n)




  /**
   * Finds Cursor position under mouse position.
   * This can be used in an OnClick handler to put the cursor at position of the mouse.
   *
   * @param x x coordinate of the position we want to find.
   * @param y y coordinate of the position we want to find.
   * @return  position of the cursor, between 1 and numLetters + 1.
   */
  def findMouseCursor(x: Double, y: Double): Int = {
    if (x < fontString.left || !this.isMouseOver(x, y)) 1
    else if (x > fontString.right) text.length + 1
    else {
      text.scanLeft(0: Double)((acc: Double, c: Char) => acc + Engine.painter.textWidth(c, fontString.font))
        .zip(1 to text.length + 1)
        .minBy((elem: (Double, Int)) => math.abs(x - (elem._1 + fontString.left)))._2
    }
  }

  /** Calls findMouseCursor(x: Double, y: Double): Int with (x,y) as the mouse position. */
  def findMouseCursor(): Int = {
    val (x, y) = Engine.mousePosition
    findMouseCursor(x, y)
  }


  /// Manage positioning
  private var _textInsets: (Double, Double, Double, Double) = (0,0,0,0) // (left, right, top, bottom)
  def textInsets: (Double, Double, Double, Double) = _textInsets

  /** Sets the offsets the text [[FontString]] should obey inside the EditBox. */
  def setTextInsets(left: Double = 0, right: Double = 0, top: Double = 0, bottom: Double = 0): Unit = {
    _textInsets = (left, right, top, bottom)
    fontString.clearAllPoints()
    fontString.setSize()
    fontString.setPoint(AnchorPoint(TopLeft, this, TopLeft, left, top))
    fontString.setPoint(AnchorPoint(BottomRight, this, BottomRight, right, bottom))
  }


  override def manageFocusSet(): Unit = {
    blinkFrame.show()
    setBlinkFrame()
  }

  override def manageClearFocus(): Unit = {
    blinkFrame.hide()
  }

  setBlinkFrame()
  blinkFrame.hide()
}


object EditBox {
  val utf8Char: Regex = "[a-z]".r // TODO: not good, to be changed
}