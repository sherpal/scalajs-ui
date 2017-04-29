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

object UIParent extends Frame("UIParent") {
  setWidth(Engine.painter.canvas.width)
  setHeight(Engine.painter.canvas.height)
  setFrameStrata(Below)
  setTopLevel()


  private def resetActualPoints(): Unit = {
    _actualPoints = List(
      (-width / 2, height / 2),
      (width / 2, height / 2),
      (-width / 2, -height / 2),
      (width / 2, height / 2)
    )
  }
  resetActualPoints()

  override def makeActualPoints(): Boolean = true

  override def getPointCoords(point: Point): Option[(Double, Double)] = Some(point match {
    case Center => center
    case TopLeft => (left, top)
    case Top => ((left + right) / 2, top)
    case TopRight => (right, top)
    case Left => (left, (top + bottom) / 2)
    case Right => (right, (top + bottom) / 2)
    case BottomLeft => (left, bottom)
    case Bottom => ((left + right) / 2, bottom)
    case BottomRight => (right, bottom)
  })

  def resize(width: Double, height: Double): Unit = {
    setWidth(width)
    setHeight(height)
    resetActualPoints()
    Region.computeAllRegionPoints()
    ScriptObject.firesScript[Region, Unit](ScriptKind.OnUIParentResize)()
  }

  def resize(): Unit = {
    resize(Engine.painter.canvas.width, Engine.painter.canvas.height)
  }
}
