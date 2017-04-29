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

/**
 * A Region is an object that can be drawn on the UI.
 * It has methods to set its [[AnchorPoint]]s, allowing to know it's position, shape and size.
 *
 * A Region can be either a Rectangle, or a Disk. A Region is a Rectangle by default, and will be a Disk if and only
 * if you set its radius explicitly, and neither the width or the height has been set manually.
 *
 * There are two types of Region, either [[Frame]] or [[LayeredRegion]].
 * A Frame typically contains the information of the Region, and a LayeredRegion are actually visible on the screen.
 */
trait Region extends ParentedObject {

  // Coordinate relative functions

  /**
   * Returns the left pixel of the Region.
   * (The (0,0) coordinate is at the center of the canvas, and left increases from left to right)
   *
   * @return left most pixel of the Region
   */
  def left: Double = {
    if (isDisk) {
      val (x, _) = center
      x - radius
    } else if (_actualPoints.isEmpty)
      0 // "random" default value to let width and height compute even if region has no _actualPoints
    else
      _actualPoints.map(elem => elem._1).min
  }

  /**
   * Returns the right most pixel of the Region.
   * (The (0,0) coordinate is at the center of the canvas, and right increases from left to right)
   *
   * @return right most pixel of the Region
   */
  def right: Double = {
    if (isDisk) {
      val (x, _) = center
      x + radius
    } else if(_actualPoints.isEmpty)
      0 // "random" default value to let width and height compute even if region has no _actualPoints
    else
      _actualPoints.map(elem => elem._1).max
  }


  /**
   * Returns the top most pixel of the Region.
   * (The (0,0) coordinate is at the center of the canvas, and top increases from bottom to top)
   *
   * @return the top pixel of the Region
   */
  def top: Double = {
    if (isDisk) {
      val (_, y) = center
      y + radius
    } else if (_actualPoints.isEmpty)
      0 // "random" default value to let width and height compute even if region has no _actualPoints
    else
      _actualPoints.map(elem => elem._2).max
  }

  /**
   * Returns the bottom most pixel of the Region.
   * (The (0,0) coordinate is at the center of the canvas, and left increases from bottom to top)
   *
   * @return the bottom most pixel of the Region
   */
  def bottom: Double = {
    if (isDisk) {
      val (_, y) = center
      y - radius
    } else if (_actualPoints.isEmpty)
      0 // "random" default value to let width and height compute even if region has no _actualPoints
    else
      _actualPoints.map(elem => elem._2).min
  }


  /**
   * Returns the coordinates of the center of the Region
   * (The (0,0) coordinate is at the center of the canvas)
   *
   * @return Couple of x coordinate and y coordinate in pixel
   */
  def center: (Double, Double) = {
    if (isRectangle) ((left + right) / 2, (top + bottom) / 2)
    else _actualPoints.head
  }


  private var _width: Option[Double] = None

  /** Returns the width of the region, in pixels */
  def width: Double = _width match {
    case None => if (isDisk) 2 * radius else right - left
    case Some(w) => w
  }

  /**
   * Sets the width of the region in pixels.
   * A region for which the width is set manually is always a Rectangle, as width
   * and height have precedence over radius or [[AnchorPoint]]s.
   * @param width the width of the region to set, in pixels
   */
  def setWidth(width: Double): Unit = {
    _width = Some(width)
    makeActualPoints()
  }
  /** Removes any width set previously */
  def setWidth(): Unit = _width = None


  private var _height: Option[Double] = None
  /** Returns the height of the region, in pixels */
  def height: Double = _height match {
    case None => if (isDisk) 2 * radius else top - bottom
    case Some(h) => h
  }
  /**
   * Sets the height of the region in pixels.
   * A region for which the height is set manually is always a Rectangle, as width
   * and height have precedence over radius or [[AnchorPoint]]s.
   * @param height the height of the region to set, in pixels
   */
  def setHeight(height: Double): Unit = {
    _height = Some(height)
    makeActualPoints()
  }
  /** Removes any height set previously */
  def setHeight(): Unit = _height = None

  /**
   * Sets the width and height of the Region.
   * A region for which the width and the height are set manually is always a Rectangle, as width
   * and height have precedence over radius or [[AnchorPoint]]s.
   * @param width  the width of the region to set, in pixels
   * @param height the height of the region to set, in pixels
   */
  def setSize(width: Double, height: Double): Unit = {
    setWidth(width)
    setHeight(height)
  }

  /**
   * Sets the width and height of the Region in such a way that width and height are equal.
   * A region for which the width and the height are set manually is always a Rectangle, as width
   * and height have precedence over radius or Points.
   * @param side side size of the square, in pixels
   */
  def setSize(side: Double): Unit = setSize(side, side)

  /** Removes the width and height previously set. */
  def setSize(): Unit = {
    setWidth()
    setHeight()
  }

  /** Returns a couple of width and height of the Region, in pixels */
  def size: (Double, Double) = (width, height)

  /** Returns a tuple of four components, left, top, width and height of the Region, respectively */
  def rect: (Double, Double, Double, Double) = (left, top, width, height)


  private var _radius: Option[Double] = None

  /**
   * Returns the radius of the Region in pixels.
   * If the Region is NOT a Disk, then returns 0. A Region is a Disk if and only if the radius has
   * been set manually, and neither the width or the height is set manually, as the width and height
   * have precedence of radius
   * @return the radius of the Disk if it is one, or 0 otherwise
   */
  def radius: Double = _radius match {
    case None => 0
    case Some(r) => r
  }

  /**
   * Sets the radius of the Region, in pixels.
   * This will make the Region a Disk only if width and height were not set manually. You can always
   * make sure it will be the case by calling region.setSize().
   * @param radius the radius of the region to set, in pixels
   */
  def setRadius(radius: Double): Unit = {
    _radius = Some(radius)
    makeActualPoints()
  }

  /** Removes any radius previously set. */
  def setRadius(): Unit = _radius = None

  /** Removes any height, width and radius previously set. */
  def clearSizes(): Unit = {
    setSize()
    setRadius()
  }

  /**
   * Returns the List of point coordinates of the Region, in pixels.
   * The (0,0) coordinate is at the center of the drawing area.
   * X coordinates increase from left to right, Y coordinates increase from bottom to top.
   */
  def getCoords: List[(Double, Double)] = _actualPoints

  /**
   * Returns the (x,y) coordinates of the specified [[Point]] of the Region, or None if
   * Region doesn't have its coordinates yet.
   * @param point the Point we want the coordinates of.
   */
  def getPointCoords(point: Point): Option[(Double, Double)] = {
    if (_actualPoints.isEmpty) None
    else {
      Some(point match {
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
    }
  }



  // Point related functions

  protected var _points: List[AnchorPoint] = List()
  protected var _actualPoints: List[(Double, Double)] = List()
  val attachedRegions: mutable.Set[Region] = mutable.Set()

  /** Returns a List of AnchorPoints of the Region. */
  def points: List[AnchorPoint] = _points

  /**
   * Removes every [[Point]] set to the Region.
   * We need to tell every region it was attached to that it is no more the case.
   */
  def clearAllPoints(): Unit = {
    _points.foreach(p => p.relativeTo.attachedRegions -= this)
    _points = List()
  }

  /** Sets all [[AnchorPoint]]s relative to its parent, or does nothing if parent is None. */
  def setAllPoints(): Unit = parent match {
    case Some(region) => setAllPoints(region)
    case _ =>
  }

  /**
   * Sets all [[AnchorPoint]]s relative to specified Region, making Region having the same position, shape and size as
   * the specified Region.
   * If the shape of the Region changes shapes from Rectangle to Disk, or the other way around, it has to be called
   * again. If radius of the specified region changes, it also has to be called again.
   * @param region the region from which to copy the shape.
   */
  def setAllPoints(region: Region): Unit = {
    if (region.isDisk) {
      setPoint(AnchorPoint(Center, region, Center))
      setRadius(region.radius)
    } else {
      setPoint(AnchorPoint(TopLeft, region, TopLeft))
      setPoint(AnchorPoint(BottomRight, region, BottomRight))
    }

    makeActualPoints()
  }

  /**
   * Adds a new [[AnchorPoint]] to draw the Region. Arguments for anchorPoint are:
   * - the [[Point]] of the Region to attach
   * - the [[Region]] to which we want to attach the point
   * - the [[Point]] of the [[Region]] to attach to
   * - xOffset [[Double]], number of horizontal pixels we want the point to be shifted
   * - yOffset [[Double]], number of vertical pixels we want the point to be shifted
   * @param anchorPoint an [[AnchorPoint]] with the wanted specifications
   */
  def setPoint(anchorPoint: AnchorPoint): Unit = {
    if (scala.scalajs.LinkingInfo.developmentMode) {
      assert(anchorPoint.relativeTo != this, "Region can't anchor to itself")
    }

    _points = anchorPoint :: _points

    anchorPoint.relativeTo.attachedRegions += this

    makeActualPoints()
  }

  /** See setPoint(anchorPoint: AnchorPoint): Unit method */
  def setPoint(point: Point): Unit =
    setPoint(AnchorPoint(point, parent.getOrElse(UIParent), point))

  /** See setPoint(anchorPoint: AnchorPoint): Unit method */
  def setPoint(point: Point, relativeTo: Region): Unit =
    setPoint(AnchorPoint(point, relativeTo, point))

  /**
   * See setPoint(anchorPoint: AnchorPoint): Unit method
   */
  def setPoint(point: Point, relativeTo: Region, relativePoint: Point, xOffset: Double = 0, yOffset: Double = 0): Unit =
    setPoint(AnchorPoint(point, relativeTo, relativePoint, xOffset, yOffset))

  /** See setPoint(anchorPoint: AnchorPoint): Unit method */
  def setPoint(point: Point, xOffset: Double, yOffset: Double): Unit =
    setPoint(AnchorPoint(point, parent.getOrElse(UIParent), point, xOffset, yOffset))


  // Family related functions
  /**
   * Sets a new Parent for the Region. A Region can't be the parent of someone, unless it's a [[Frame]].
   * A [[LayeredRegion]] has to be inserted in the Parent draw order.
   * If the [[Frame]] was the descendant of a [[ScrollFrame]], we need to tell the Region and its descendant.
   * @param frame new Parent.
   */
  def setParent(frame: Frame): Unit = {

    removeParent()

    _parent = Some(frame)

    if (scala.scalajs.LinkingInfo.developmentMode) {
      /**
       * Returns whether specified region is not a descendant of this Region.
       * This function is not used in production code, as it should never happen.
       */
      def noParentCycle(region: Region): Boolean = {
        var current: Option[Region] = region.parent
        while (current.isDefined) {
          assert(!(current.get == this), "region became its own ancestor!")
          current = current.get.parent
        }
        true
      }
      noParentCycle(frame)
    }

    this match {
      case fs: FontString =>
        fs.setDrawLayer(fs.drawLayer, 9)
        frame.orderLayeredRegions()
      case _: Texture => frame.orderLayeredRegions()
      case frame: Frame => frame.parentChanged()
      case _ =>
    }

    frame.children += this

    getScrollFrameAncestor match {
      case Some(scrollFrame) =>
        descendants.foreach(_._scrollFrameAncestor = Some(scrollFrame))
        scrollFrame.updateScrollChildRect()
      case _ =>
    }
  }

  /** Sets the new Parent as [[UIParent]] */
  def setParent(): Unit = setParent(UIParent)

  /** Remove the current Parent */
  def removeParent(): Unit = {
    if (parent.isDefined) {
      parent.get.children -= this
      this match {
        case tex: Texture => parent.get.removeTexture(tex)
        case fs: FontString => parent.get.removeFontString(fs)
        case _ =>
      }
    }
    descendants.foreach(_.setScrollFrameAncestor(None))
  }

  /**
   * Returns a List of all the ancestors of the Region.
   * The first element of the List is the closest parent, the last element is the further away.
   */
  def ancestors: List[Frame] = {
    var anc: List[Frame] = List()
    var ancestor: Option[Frame] = parent
    while (ancestor.isDefined) {
      anc = ancestor.get :: anc
      ancestor = ancestor.get.parent
    }

    anc.reverse
  }

  protected var _scrollFrameAncestor: Option[ScrollFrame] = None
  /**
   * Returns the closest [[ScrollFrame]] corresponding to the description below, or None.
   * Checking if an ancestor is the scroll child of some [[ScrollFrame]] and returning its parent, if relevant
   */
  def getScrollFrameAncestor: Option[ScrollFrame] =
    ancestors.find(f => f.parent.isDefined && (f.parent.get match {
      case scrollFrame: ScrollFrame => scrollFrame.getScrollChild == f
      case _ => false
    })) match {
      case Some(f) => Some(f.parent.get.asInstanceOf[ScrollFrame])
      case _ => None
    }

  /** Sets the [[ScrollFrame]] ancestor. May be None. */
  def setScrollFrameAncestor(scrollFrame: Option[ScrollFrame]): Unit = _scrollFrameAncestor = scrollFrame



  // Asking Region properties

  /** Returns whether the Region has enough information to be drawn. */
  def canBeDrawn: Boolean = {
    _actualPoints.length > 2 || (isDisk && _actualPoints.length == 1)
  }

  /** Returns whether Region is certainly a Disk. */
  def isDisk: Boolean = _radius.isDefined && (_width, _height) == (None, None)

  /** Returns whether Region is certainly a Rectangle. */
  def isRectangle: Boolean = (_width, _height) match {
    case (None, None) => _points.exists(p => p.point != Center)
    case _ => true
  }

  /** Returns whether the (x,y) coordinate of the screen is inside the Region. */
  def isMouseOver(x: Double, y: Double): Boolean = {
    canBeDrawn && {
      if (isRectangle) x > left && x < right && y < top && y > bottom
      else {
        val (cx, cy) = center
        (cx - x) * (cx - x) + (cy - y) * (cy - y) < radius * radius
      }
    }
   }


  /// originally in Visible Region

  private var _visible: Boolean = true

  // Alpha related functions

  private var _alpha: Double = 1.0

  /** Returns manually set alpha of the Region. */
  def alpha: Double = _alpha

  /** Returns the alpha at which the Region must be drawn. */
  def getEffectiveAlpha: Double = {
    parent match {
      case Some(p) => p.getEffectiveAlpha * alpha
      case _ => alpha
    }
  }

  /** Sets the alpha of the Region, clamped to [0,1] */
  def setAlpha(alpha: Double): Unit =
    _alpha = if (alpha < 0) 0.0 else if (alpha > 1) 1.0 else alpha

  /** Sets the alpha to 1 */
  def setAlpha(): Unit =
    _alpha = 1.0

  // Asking Region visibility properties

  /**
   * Returns whether Region was set to Visible.
   * This does not imply that the Region is Visible.
   */
  def isShown: Boolean = _visible

  /**
   * Returns whether the Region is Visible.
   * It is Visible if and only if it is shown and its parent is Visible, or has no parent.
   */
  def isVisible: Boolean = (_scrollFrameAncestor match {
    case Some(scrollFrame) => scrollFrame.amIVisible(this)
    case _ => true
  }) && (parent match {
    case Some(p) => _visible && canBeDrawn && p.isVisible
    case _ => _visible
  })

  // Hide and Show

  /**
   * Hides the Region.
   * Triggers OnHide Script for itself and every [[Frame]] descendant if they was Visible before the call.
   */
  def hide(): Unit = {
    this match {
      case frame: Frame => frame.storeInHidden()
      case _ =>
    }

    descendants.filter({
      case child: ScriptObject => child.hasScript(ScriptKind.OnHide)
      case _ => false
    }).foreach({
      case child: Region if child.isVisible => child.asInstanceOf[ScriptObject].fires[Region, Unit](ScriptKind.OnHide)()
      case _ =>
    })

    _visible = false
  }

  /**
   * Show the Region.
   * Triggers OnShow Script for itself and every [[Frame]] descendant if they become Visible.
   */
  def show(): Unit = {
    _visible = true

    this match {
      case frame: Frame => frame.unStoreFromHidden()
      case _ =>
    }

    descendants.filter({
      case child: ScriptObject => child.hasScript(ScriptKind.OnShow)
      case _ => false
    }).foreach({
      case child: Region if child.isVisible => child.asInstanceOf[ScriptObject].fires[Region, Unit](ScriptKind.OnShow)()
      case _ =>
    })
  }



  // helper functions

  /**
   * Finds the coordinates that can be deduced from the known coordinates.
   * Example:
   * If TopLeft and width are known, then we can deduce the TopRight coordinate.
   *
   * /!\ This function is only intended to be called inside makeActualPoints method, but put outside for the sake of
   * clarity.
   * @param knownPoints a mutable map that remembers all the points known up to now
   */
  private def addDeducedPoints(knownPoints: mutable.Map[Point, (Double, Double)]): Unit = {
    var bottom: Option[Double] = None
    var top: Option[Double] = None
    var left: Option[Double] = None
    var right: Option[Double] = None
    var centerX: Option[Double] = None
    var centerY: Option[Double] = None

    knownPoints.foreach(elem => {
      val point: Point = elem._1
      val x: Double = elem._2._1
      val y: Double = elem._2._2

      if (point == BottomRight || point == Bottom || point == BottomLeft) bottom = Some(y)
      else if (point == TopRight || point == Top || point == TopLeft) top = Some(y)
      else centerY = Some(y)

      if (point == TopLeft || point == Left || point == BottomLeft) left = Some(x)
      else if (point == TopRight || point == Right || point == BottomRight) right = Some(x)
      else centerX = Some(x)
    })

    var h: Option[Double] = _height
    _height match {
      case None => (bottom, top, centerY) match {
        case (Some(b), Some(t), _) => h = Some(t - b)
        case (Some(b), _, Some(c)) => h = Some(2 * (c - b))
        case (_, Some(t), Some(c)) => h = Some(2 * (t - c))
        case _ =>
      }
      case _ =>
    }

    var w: Option[Double] = _width
    _width match {
      case None => (left, right, centerX) match {
        case (Some(l), Some(r), _) => w = Some(r - l)
        case (Some(l), _, Some(c)) => w = Some(2 * (c - l))
        case (_, Some(r), Some(c)) => w = Some(2 * (r - c))
        case _ =>
      }
      case _ =>
    }

    (w, h) match {
      case (Some(width), Some(height)) =>
        if (knownPoints.isDefinedAt(Center)) {
          val (x, y) = knownPoints(Center)
          _actualPoints = List(
            (x - width / 2, y + height / 2), (x + width / 2, y + height / 2),
            (x - width / 2, y - height / 2), (x + width / 2, y - height / 2)
          )
        } else if (knownPoints.isDefinedAt(TopLeft)) {
          val (x, y) = knownPoints(TopLeft)
          _actualPoints = List(
            (x, y), (x + width, y), (x, y - height), (x + width, y - height)
          )
        } else if (knownPoints.isDefinedAt(Top)) {
          val (x, y) = knownPoints(Top)
          _actualPoints = List(
            (x - width / 2, y), (x + width / 2, y), (x - width / 2, y - height), (x + width / 2, y - height)
          )
        } else if (knownPoints.isDefinedAt(TopRight)) {
          val (x, y) = knownPoints(TopRight)
          _actualPoints = List(
            (x - width, y), (x, y), (x - width, y - height), (x, y - height)
          )
        } else if (knownPoints.isDefinedAt(Left)) {
          val (x, y) = knownPoints(Left)
          _actualPoints = List(
            (x, y + height / 2), (x + width, y + height / 2), (x, y - height / 2), (x + width, y - height / 2)
          )
        } else if (knownPoints.isDefinedAt(Right)) {
          val (x, y) = knownPoints(Right)
          _actualPoints = List(
            (x - width, y + height / 2), (x, y + height / 2), (x - width, y - height / 2), (x, y - height / 2)
          )
        } else if (knownPoints.isDefinedAt(BottomLeft)) {
          val (x, y) = knownPoints(BottomLeft)
          _actualPoints = List(
            (x, y + height), (x + width, y + height), (x, y), (x + width, y)
          )
        } else if (knownPoints.isDefinedAt(Bottom)) {
          val (x, y) = knownPoints(Bottom)
          _actualPoints = List(
            (x - width / 2, y + height), (x + width / 2, y + height), (x - width / 2, y), (x + width / 2, y)
          )
        } else if (knownPoints.isDefinedAt(BottomRight)) {
          val (x, y) = knownPoints(BottomRight)
          _actualPoints = List(
            (x - width, y + height), (x, y + height), (x - width, y), (x, y)
          )
        }
      case _ =>
    }
  }

  /**
   * Fills the actual_points array of Region
   * This function has to be called after every change in coordinates.
   * This may happen if a new point is set, or there was a modification in parent,
   * or the width, height or radius has been explicitly changed.
   * This method may NOT be called if it wasn't for the parent region already.
   * -- If region is a rectangle, we use the minimum information we need, starting with
   *   -- width and height, and following by points set.
   *   -- The information are gathered until three non aligned points are found.
   *
   * @return true if the region can now be drawn, false otherwise.
   */
  protected def makeActualPoints(): Boolean = {
    // checking if there are points before deleting previous actual points
    if (_points.isEmpty) false
    else if (isRectangle) {
      val knownPoints: mutable.Map[Point, (Double, Double)] = mutable.Map()
      _actualPoints = List()

      _points.foreach(p => if (!knownPoints.isDefinedAt(p.point)) {
        p.relativeTo.getPointCoords(p.relativePoint) match {
          case None =>
          case Some((xParent, yParent)) =>
            val x = xParent + p.xOffset
            val y = yParent + p.yOffset
            knownPoints += (p.point -> (x, y))
            addDeducedPoints(knownPoints)
        }
      })

      if (_actualPoints.length == 4) {
        attachedRegions.foreach(region => region.makeActualPoints())
        true
      } else false
    } else {
      _points.find(p => p.point == Center) match {
        case Some(point) =>
          point.relativeTo.getPointCoords(point.relativePoint) match {
            case Some((xParent, yParent)) =>
              val x = xParent + point.xOffset
              val y = yParent + point.yOffset

              _actualPoints = List((x, y))

              attachedRegions.foreach(region => region.makeActualPoints())

              true
            case _ => false
          }
        case _ => false
      }
    }
  }
}


object Region {
  def computeAllRegionPoints(): Unit = {
    UIParent.children.foreach(_.makeActualPoints())
  }
}

