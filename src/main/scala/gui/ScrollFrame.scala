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

import scala.collection.mutable


class ScrollFrame(n: String = "", parent: Option[Frame] = Some(UIParent)) extends Frame(n, parent) {


  private var _horizontalScroll: Double = 0

  /** Returns the horizontal scroll value. */
  def horizontalScroll: Double = _horizontalScroll

  /**
   * Sets the horizontal scroll value, clamped between 0 and _horizontalScrollRange.
   * Triggers the OnHorizontalScroll script, regardless whether the value actually changed.
   */
  def setHorizontalScroll(scroll: Double): Unit = {
    val prevValue = _horizontalScroll
    _horizontalScroll = if (scroll < 0) 0 else if (scroll > _horizontalScrollRange) _horizontalScrollRange else scroll

    updateScrollFrame()

    fires[ScrollFrame, Double, Double, Unit](ScriptKind.OnHorizontalScroll)(_horizontalScroll, prevValue)
  }

  private var _horizontalScrollRange: Double = 0

  /**
   * Returns the range of the horizontal scroll.
   * There is no method to set it manually, as it has to be handled automatically.
   */
  def horizontalScrollRange: Double = _horizontalScrollRange


  private var _verticalScroll: Double = 0

  /** Returns the vertical scroll value. */
  def verticalScroll: Double = _verticalScroll

  /**
   * Sets the vertical scroll value, clamped between 0 and _verticalScrollRange.
   * Triggers the OnVerticalScroll script, regardless whether the value actually changed.
   */
  def setVerticalScroll(scroll: Double): Unit = {
    val prevValue = _verticalScroll
    _verticalScroll = if (scroll < 0) 0 else if (scroll > _verticalScrollRange) _verticalScrollRange else scroll

    updateScrollFrame()

    fires[ScrollFrame, Double, Double, Unit](ScriptKind.OnVerticalScroll)(_verticalScroll, prevValue)
  }

  private var _verticalScrollRange: Double = 0

  /**
   * Returns the range of the vertical scroll.
   * There is no method to set it manually, as it has to be handled automatically.
   */
  def verticalScrollRange: Double = _verticalScrollRange

  /**
   * Calls updateScrollFrame method.
   * This allows updateScrollFrame to be called from the outside, as the user may need to adjust ScrollFrame manually.
   */
  def updateScrollChildRect(): Unit = updateScrollFrame()


  private var scrollChild: Frame = new Frame("", Some(this))
  scrollChild.setSize(10,10)

  /** Returns the scroll child attached to the ScrollFrame. */
  def getScrollChild: Frame = scrollChild

  /**
   * Attaches the child to the ScrollFrame.
   *
   * @param child the frame to attach, that must be a Rectangle. We do not check it in production code.
   */
  def setScrollChild(child: Frame): Unit = {
    if (scala.scalajs.LinkingInfo.developmentMode) {
      assert(child.isRectangle, "child of scroll frame must be a rectangle")
    }

    scrollChild.removeParent()
    scrollChild.enableMouse(enable = true)

    scrollChild = child
    child.enableMouse()
    scrollChild.setParent(this)
    scrollChild.descendants.foreach(_.setScrollFrameAncestor(Some(this)))

    updateScrollFrame()
  }

  /** Returns whether the specified region is (partly) Visible from the ScrollFrame. */
  def amIVisible(descendant: Region): Boolean = visibleChildren.contains(descendant)


  /// helper functions

  private def updateScrollFrame(): Unit = {

    val prevH = _horizontalScrollRange
    _horizontalScrollRange = math.max(0, scrollChild.width - width)
    val prevV = _verticalScrollRange
    _verticalScrollRange = math.max(0, scrollChild.height - height)

    if (prevH != _horizontalScrollRange || prevV != _verticalScrollRange) {
      fires[ScrollFrame, Double,
        Double, Unit](ScriptKind.OnScrollRangeChanged)(_horizontalScrollRange, _verticalScrollRange)
    }

    scrollChild.clearAllPoints()

    scrollChild.setPoint(AnchorPoint(TopLeft, this, TopLeft, -horizontalScroll, verticalScroll))

    setVisibleChildren()
  }

  private val visibleChildren: mutable.Set[Region] = mutable.Set()
  private def setVisibleChildren(): Unit = {
    visibleChildren.clear()
    scrollChild.descendants.foreach(r => {
      if (r.canBeDrawn && r.bottom < top && r.top > bottom && r.right > left && r.left < right) visibleChildren += r
    })
  }
}
