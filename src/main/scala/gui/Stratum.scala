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



sealed abstract class Stratum extends Ordered[Stratum] {
  val n: Int

  def compare(that: Stratum): Int = this.n - that.n
}


// The Below Stratum should be used only by UIParent object
case object Below extends Stratum {
  val n: Int = 0
}

case object Background extends Stratum {
  val n: Int = 1
}

case object Low extends Stratum {
  val n: Int = 2
}

case object Medium extends Stratum {
  val n: Int = 3
}

case object High extends Stratum {
  val n: Int = 4
}

case object Dialog extends Stratum {
  val n: Int = 5
}

case object Fullscreen extends Stratum {
  val n: Int = 6
}

case object FullscreenDialog extends Stratum {
  val n: Int = 7
}

case object TooltipStratum extends Stratum {
  val n: Int = 8
}
