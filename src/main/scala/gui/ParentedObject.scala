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


trait ParentedObject extends UIObject {
  protected var _parent: Option[Frame]

  /** Returns the parent of the Object or None if it has no parent. */
  def parent: Option[Frame] = _parent

  protected val children: mutable.Set[Region] = mutable.Set[Region]()

  /**
   * Returns a set of Children.
   * The mutable.Set children protected value is sent to a Set so that it can't be modified from the outside.
   */
  def getChildren: Set[Region] = children.toSet

  /**
   * Returns an Iterator looping on
   * - this Object, as a Region
   * - children of this Object
   * - recursive call on children of this Object
   */
  def descendants: Iterator[Region] = Iterator(this.asInstanceOf[Region]) ++ children.flatMap(_.descendants)
}
