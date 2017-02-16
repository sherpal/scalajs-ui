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

package gameengine



class GameState(val load: () => Unit = () => {},
                val update: (Double) => Unit = (dt: Double) => {},
                val draw: () => Unit = () => {},
                val keyPressed: (String, Boolean) => Unit = (key: String, isRepeat: Boolean) => {},
                val keyReleased: (String) => Unit = (key: String) => {},
                val mousePressed: (Double, Double, Int) => Unit = (x: Double, y: Double, button: Int) => {},
                val mouseReleased: (Double, Double, Int) => Unit = (x: Double, y: Double, button: Int) => {},
                val mouseMoved: (Double, Double, Double, Double, Int) => Unit =
                (x: Double, y: Double, dx: Double, dy: Double, button: Int) => {},
                val mouseWheel: (Int, Int, Int) => Unit = (dx: Int, dy: Int, dz: Int) => {},
                val run: Option[() => Unit] = None)


