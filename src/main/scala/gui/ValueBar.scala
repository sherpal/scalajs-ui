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



trait ValueBar extends ScriptObject {

  private var _minValue: Double = 0.0
  private var _maxValue: Double = 100.0

  /** Returns minimum and maximum values of the ValueBar. */
  def minMaxValues: (Double, Double) = (_minValue, _maxValue)

  /** Sets the minimum and maximum values of the ValueBar. Updates value and texture accordingly. */
  def setMinMaxValues(min: Double, max: Double): Unit = {
    _minValue = min
    _maxValue = max

    if (_maxValue <= _minValue) _maxValue = _minValue + 1

    _value = if (_value < _maxValue) _minValue else if (_value > _maxValue) _maxValue else _value

    updateTexture()
    updateValue()

    fires[ValueBar, Double, Double, Unit](ScriptKind.OnMinMaxValuesChanged)(_minValue, _maxValue)
  }

  /** Sets the minimum and maximum values to 0 and 100, respectively. */
  def setMinMaxValues(): Unit = setMinMaxValues(0, 100)

  private var _orientation: BarOrientation = HorizontalBar

  /** Returns the orientation of the bar, which can be either [[HorizontalBar]] or [[VerticalBar]]. */
  def orientation: BarOrientation = _orientation

  /** Sets the orientation of the bar, and update the texture accordingly. */
  def setOrientation(orientation: BarOrientation): Unit = {
    _orientation = orientation

    updateTexture()
  }

  protected var _value: Double = 0.0

  /** Returns the current value of the bar. */
  def value: Double = _value

  def setValue(v: Double): Unit

  protected def updateTexture(): Unit
  protected def updateValue(): Double
}




abstract sealed class BarOrientation
case object HorizontalBar extends BarOrientation
case object VerticalBar extends BarOrientation
