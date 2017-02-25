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

import scala.collection.mutable


class Frame(n: String = "", par: Option[Frame] = None) extends ScriptObject with Region {

  def this(frame: Frame) = this("", Some(frame))

  val name: String = UIObject.setName(n)
  protected var _parent: Option[Frame] = par


  /// Family related functions

  /**
   * Creates and returns a new [[FontString]] child for the Frame.
   * The FontString is inserted into the draw order of the Frame.
   */
  def createFontString(name: String = ""): FontString = {
    val fs = new FontString(name, this)
    _orderedLayeredRegions = fs +: _orderedLayeredRegions
    orderLayeredRegions()

    fs
  }

  /** Removes a [[FontString]] child. */
  def removeFontString(fs: FontString): Unit = {
    _orderedLayeredRegions = _orderedLayeredRegions.filter(_ != fs)
    orderLayeredRegions() // should not be necessary
  }

  /**
   * Creates and returns a new [[Texture]] child for the Frame.
   * The Texture is inserted into the draw order of the Frame.
   */
  def createTexture(name: String = "", layer: Layer = Artwork, subLayer: Int = 0): Texture = {
    val tex = new Texture(name, this)
    _orderedLayeredRegions = tex +: _orderedLayeredRegions
    tex.setDrawLayer(layer, subLayer)

    tex
  }

  /** Removes a [[Texture]] child. */
  def removeTexture(tex: Texture): Unit = {
    _orderedLayeredRegions = _orderedLayeredRegions.filter(_ != tex)
    orderLayeredRegions()
  }

  /** Returns a set of non [[Frame]] children. */
  def regions: Set[Region] = children.filter(_ match {
    case _: Frame => false
    case _ => true
  }).toSet

  /** Calls the draw method of all the Visible [[LayeredRegion]] children, in the order they have to be. */
  def drawChildren(): Unit = layeredChildren.filter(_.isVisible).foreach(_.draw())

  /** Returns the List of [[LayeredRegion]] children, in the order they have to be drawn. */
  def layeredChildren: List[LayeredRegion] = _orderedLayeredRegions

  /**
   * Adapt the stratum to new parent stratum, if it was on the same stratum of its previous parent.
   * Removes top level.
   */
  def parentChanged(): Unit = {
    parent match {
      case Some(p) if frameStrata == p.frameStrata =>
        this.setFrameLevel(p.frameLevel)
        Frame.setDrawOrder(frameStrata)
        setTopLevel()
      case _ =>
    }
  }

  /// Stratum and layers related functions

  private var _level: Int = 0

  /** Returns the frame level of the Frame. */
  def frameLevel: Int = _level

  /** Sets the frame level of the Frame. The higher the level, the later it is drawn. */
  def setFrameLevel(level: Int, noNeedUpdate: Boolean = false): Unit = {
    _level = level
    /*
     * need only change current stratum
     * noNeedUpdate is true when we know that setDrawOrder will be called
     * later, e.g., when calling in putOnTop method
     */
    if (!noNeedUpdate) Frame.setDrawOrder(this.frameStrata)
  }

  private var _stratum: Stratum = Medium

  /** Returns the stratum of the Frame. */
  def frameStrata: Stratum = _stratum

  /**
   * Sets the new stratum of the Frame.
   * Updates previous and new strata draw orders.
   */
  def setFrameStrata(stratum: Stratum): Unit = {
    val prevStratum = frameStrata
    Frame.strata(prevStratum) -= this
    Frame.setDrawOrder(prevStratum) // update previous stratum

    _stratum = stratum
    Frame.strata(frameStrata) += this
    Frame.setDrawOrder(frameStrata) // update new stratum
  }

  /// Top level related functions

  private var _topLevel: Boolean = false

  /** Returns whether Frame is on top of its [[Stratum]]. */
  def isOnTop: Boolean =
    Frame.strata(this.frameStrata).union(Frame.hidden.filter(f => f.frameStrata == this.frameStrata)).filter({
      f => !(f.ancestors.contains(this) || f == this)
    }).forall(f => f.frameLevel < this.frameLevel)

  /** Returns whether the Frame has to be put on top when clicked on. */
  def isTopLevel: Boolean = _topLevel

  /** Put the Frame on top of its [[Stratum]] if it is not already. */
  def putOnTop(noNeedUpdate: Boolean = false): Unit = if (!isOnTop) {
    val maxLevel = Frame.strata(this.frameStrata).union(Frame.hidden.filter(_.frameStrata == this.frameStrata))
      .map(_.frameLevel).max

    setFrameLevel(maxLevel + 1, noNeedUpdate = true)
    putChildrenOnSameLevel()

    if (!noNeedUpdate) Frame.setDrawOrder(frameStrata)
  }

  /** Enables or Disables top level behaviour. */
  def setTopLevel(enable: Boolean = false): Unit = _topLevel = enable

  /// Event related functions

  private val registeredEvents: mutable.Set[ScriptKind] = mutable.Set()

  /**
   * Sets all events to be registered.
   * This does not put any handler to any event.
   *
   * Mainly used for testing purposes.
   */
  def registerAllEvents(): Unit =
    ScriptObject.possibleEvents.foreach(registeredEvents += _)

  /** Registers an event and associated an corresponding handler to it. */
  def registerEvent(event: ScriptKind)(handler: event.Handler): Unit = {
    ScriptObject.framesWithEventByKind(event) += this
    handlers += (event -> handler)
    registeredEvents += event
  }

  /** Removes all registered events, forgetting they associated handlers. */
  def unregisterAllEvents(): Unit = {
    registeredEvents.foreach(event => {
      ScriptObject.framesWithEventByKind(event) -= this
      handlers -= event
    })
    registeredEvents.clear()
  }

  /** Unregister a particular event. */
  def unregisterEvent(event: ScriptKind): Unit = {
    ScriptObject.framesWithEventByKind(event) -= this
    handlers -= event
    registeredEvents -= event
  }

  /// Mouse related functions

  private var _mouseEnabled: Boolean = true

  /** Allows or prevent Frame to respond to click events. */
  def enableMouse(enable: Boolean = false): Unit =
    _mouseEnabled = enable

  /** Returns whether Frame is set to respond to click events. */
  def isMouseEnabled: Boolean = _mouseEnabled

  /** Returns whether Frame will respond to click events. */
  def sensibleToScript: Boolean = isShown && (parent match {
    case Some(p) => p.asInstanceOf[Frame].sensibleToScript
    case _ => true
  })

  /// Drag related functions

  /**
   * The clamped frame restrain this Frame liberty of movement within the boundary of the clamped frame.
   * This ensures that the Frame can't escape some space of the screen while dragging.
   */
  private var _clamped: Option[Frame] = None

  /** Returns whether Frame is constrained into a given area. */
  def clamped: Boolean = _clamped.isDefined

  /** Sets the clamped frame, which can't be itself (not checking that at production code). */
  def setClampedToFrame(frame: Frame): Unit = {
    if (scala.scalajs.LinkingInfo.developmentMode) {
      assert(frame != this, "frame can't clamp to itself")
    }

    _clamped = Some(frame)
  }

  /** Sets [[UIParent]] as the clamped frame. */
  def setClampedToScreen(enable: Boolean): Unit = {
    if (enable) setClampedToFrame(UIParent)
    else _clamped = None
  }

  /** Returns whether the clamped frame is define and whether it is UIParent. */
  def isClampedToScreen: Boolean = _clamped match {
    case Some(UIParent) => true
    case _ => false
  }

  /** Returns the clamped frame, or None, whichever is relevant. */
  def clamp: Option[Frame] = _clamped

  /**
   * Returns whether the Frame is registered for some drag, either for itself or for an other one.
   */
  def isRegisteredForDrag: Boolean = draggingInfo.isDefined

  private var draggingInfo: Option[DraggingInfo] = None
  private var dragEventInfo: Option[DragEventInfo] = None

  /**
   * Registers the Frame to be dragged from a rectangular sub-region.
   *
   * @param tlx    X coordinate of the top left point of the drag enabled area,
   *               with respect to the top left point of the Frame.
   * @param tly    Y coordinate of the top left point of the drag enabled area,
   *               with respect to the top left point of the Frame.
   * @param brx    X coordinate of the bottom right point of the drag enabled area,
   *               with respect to the top left point of the Frame.
   * @param bry    Y coordinate of the bottom right point of the drag enabled area,
   *               with respect to the top left point of the Frame.
   * @param button the number of the button that must activate the drag, default to 0.
   */
  def registerForDrag(tlx: Double, tly: Double, brx: Double, bry: Double, button: Int = 0): Unit = {
    unregisterForDrag()
    draggingInfo = Some(new RectangularDragging(button, tlx, tly, brx, bry))
  }

  /**
   * Registers the Frame to be dragged from a circular sub-region.
   *
   * @param cx     X coordinate of the center of the drag region, with respect to top left of the Frame.
   * @param cy     Y coordinate of the center of the drag region, with respect to top left of the Frame.
   * @param radius radius of the drag region, in pixels.
   * @param button the number of the button that must activate the drag, default to 0.
   */
  def registerForDrag(cx: Double, cy: Double, radius: Double, button: Int): Unit = {
    unregisterForDrag()
    draggingInfo = Some(new CircularDragging(button, cx, cy, radius))
  }
  def registerForDrag(cx: Double, cy: Double, radius: Double): Unit =
    registerForDrag(cx, cy, radius, 0)

  /**
   * Registers the Frame to be dragged from an other frame, preferably a child or itself.
   *
   * @param frame  the Frame that must trigger the drag.
   * @param button the number of the button that must activate the drag, default to 0.
   */
  def registerForDrag(frame: Frame, button: Int): Unit = {
    unregisterForDrag()
    if (frame == this) {
      draggingInfo = Some(new FrameDragging(button, this))
    } else {
      draggingInfo = Some(new FrameDragging(button, this))
      frame.draggingInfo = Some(new SubFrameDragging(button, this))
    }
  }
  def registerForDrag(frame: Frame): Unit = {
    unregisterForDrag()
    if (frame == this) {
      draggingInfo = Some(new FrameDragging(0, this))
    } else {
      draggingInfo = Some(new FrameDragging(0, this))
      frame.draggingInfo = Some(new SubFrameDragging(0, this))
    }
  }

  /** Removing any dragging information of the Frame, and its linked frame if any. */
  def unregisterForDrag(): Unit = if (draggingInfo.isDefined) {
    val info = draggingInfo.get
    draggingInfo = None // removing info now, otherwise this will cause infinite loop.
    info match {
      case drag: FrameDragging => drag.frame.unregisterForDrag()
      case drag: SubFrameDragging => drag.frame.unregisterForDrag()
      case _ =>
    }
  }

  /// texture ordering related functions

  private var _orderedLayeredRegions: List[LayeredRegion] = List()

  /** Sorts the [[LayeredRegion]] children by order they have to be drawn. */
  def orderLayeredRegions(): Unit =
    _orderedLayeredRegions = _orderedLayeredRegions.sortWith((t1, t2) => compareLayeredRegionDrawOrder(t1, t2))


  /// store in hidden

  /**
   * Storing Frame in hidden layer, so that we don't bother sorting it while finding draw order.
   * This method is called when hiding a Frame.
   *
   * @param noNeedUpdate should always be false when called from the outside.
   */
  def storeInHidden(noNeedUpdate: Boolean = false): Unit = {
    def store(frame: Frame): Unit = {
      Frame.hidden += frame
      Frame.strata(frame.frameStrata) -= this
    }

    descendants.foreach {
      case child: Frame => store(child)
      case _ =>
    }

    if (!noNeedUpdate) Frame.setDrawOrder()
  }

  /**
   * Restoring Frame into its stratum, in order to be drawn again.
   *
   * @param noNeedUpdate should always be false when called from the outside.
   */
  def unStoreFromHidden(noNeedUpdate: Boolean = false): Unit = {
    def unStore(frame: Frame): Unit = {
      Frame.hidden -= frame
      Frame.strata(frame.frameStrata) += frame
    }

    descendants.foreach({
      case child: Frame => unStore(child)
      case _ =>
    })

    if (!noNeedUpdate) Frame.setDrawOrder()
  }


  par match {
    case Some(parent) =>
      setParent(parent)
      setFrameStrata(parent.frameStrata)
      setFrameLevel(parent.frameLevel)
    case _ =>
      setFrameStrata(frameStrata)
  }


  ////////////////////////////////////////////////////////////////////////////////////////
  /// Helper functions ///
  ////////////////////////

  /** Puts every Frame children at the same level as the Frame, when Frame is put on top. */
  private def putChildrenOnSameLevel(): Unit = {
    children.foreach({
      case child: Frame =>
        child.setFrameLevel(this.frameLevel)
        child.putChildrenOnSameLevel()
      case _ =>
    })
  }

  /** Compares two [[LayeredRegion]]s by draw order. */
  private def compareLayeredRegionDrawOrder(t1: LayeredRegion, t2: LayeredRegion): Boolean = {
    if (t1.drawLayer < t2.drawLayer) true
    else if (t1.drawLayer > t2.drawLayer) false
    else t1.drawSubLayer < t2.drawSubLayer
  }
}


object Frame {


  private var _topMouseFrame: Frame = _


  private val frameDrawOrder: mutable.Map[Stratum, List[Frame]] = mutable.Map(
    Below -> List[Frame](),
    Background -> List[Frame](), Low -> List[Frame](), Medium -> List[Frame](), High -> List[Frame](),
    Dialog -> List[Frame](), Fullscreen -> List[Frame](), FullscreenDialog -> List[Frame](),
    TooltipStratum -> List[Frame]()
  )

  private val strata: Map[Stratum, mutable.Set[Frame]] = Map(
    Below -> mutable.Set(),
    Background -> mutable.Set(), Low -> mutable.Set(), Medium -> mutable.Set(), High -> mutable.Set(),
    Dialog -> mutable.Set(), Fullscreen -> mutable.Set(), FullscreenDialog -> mutable.Set(),
    TooltipStratum -> mutable.Set()
  )

  private val hidden: mutable.Set[Frame] = mutable.Set[Frame]()

  /** Draws every visible frame on the screen. */
  def drawAllFrames(): Unit = List(Below, Background, Low, Medium, High, Dialog, Fullscreen, FullscreenDialog,
    TooltipStratum)
    .foreach(frameDrawOrder(_).foreach(_.drawChildren()))

  /** returns true if f1 must be drawn before (i.e. is below). */
  private def compareDrawOrder(f1: Frame, f2: Frame): Boolean = {
    if (f1.frameStrata < f2.frameStrata) true
    else if (f2.frameStrata < f1.frameStrata) false
    else {
      if (f1.frameLevel < f2.frameLevel) true
      else if (f2.frameLevel < f1.frameLevel) false
      else {
        /*
         * checking if one is ancestor of the other
         * if f1 is ancestor of f2, then it must be drawn before
         * otherwise, it does not matter
         */
        f2.ancestors.contains(f1)
      }
    }
  }

  private def insertFrameDrawOrder(f: Frame, layer: Stratum): Unit = {
    def insert(elem: Frame, list: List[Frame]): List[Frame] = {
      if (list.isEmpty) List(elem)
      else if (compareDrawOrder(elem, list.head)) elem :: list
      else list.head :: insert(elem, list.tail)
    }

    frameDrawOrder += (layer -> insert(f, frameDrawOrder(layer)))
  }

  /**
   * Sorts frames in order in which they must be drawn.
   * This function has to be called any time the draw order is changed.
   * It may happen, e.g., if a new frame is created (will go top of its stratum),
   * if stratum of a frame is changed, if a frame with topLevel = true is clicked...
   *
   * @param layer the layer in which to sort the frame.
   */
  def setDrawOrder(layer: Stratum): Unit = {
    frameDrawOrder += (layer -> List())
    strata(layer).foreach(f => insertFrameDrawOrder(f, layer))
  }

  /** Does the same as setDrawOrder(layer: Stratum): Unit but for every [[Stratum]]. */
  def setDrawOrder(): Unit = strata.foreach(elem => setDrawOrder(elem._1))

  /** Returns the Frame that is just beneath the mouse */
  def topMouseFrame: Frame = _topMouseFrame

  /** Returns the top most frame under the mouse, if it is mouseEnabled and Visible. */
  def topUnderMouse(mouseX: Double, mouseY: Double): Frame = {
    def findLast[T](list: List[T], predicate: (T) => Boolean): Option[T] = {
      list.foldLeft(None: Option[T])((b: Option[T], elem: T) => {
        if (predicate(elem)) Some(elem)
        else b
      })
    }

    //List(Below, Background, Low, Medium, High, Dialog, Fullscreen, FullscreenDialog, TooltipStratum)
    List(TooltipStratum, FullscreenDialog, Fullscreen, Dialog, High, Medium, Low, Background, Below)
      .find(strata(_).exists(f => f.isMouseEnabled && f.isMouseOver(mouseX, mouseY) && f.isVisible)) match {
      case Some(stratum) =>
        findLast[Frame](frameDrawOrder(stratum), f => f.isMouseEnabled && f.isMouseOver(mouseX, mouseY) && f.isVisible)
        match {
          case Some(frame) => frame
          case _ => UIParent
        }
      case _ => UIParent
    }
  }

  /**
   * Updates the frame that is top most under the mouse.
   * This triggers OnEnter and OnLeave scripts if the topMouseFrame has changed.
   * It also sets or removes highlight of entered or left button, if relevant.

   * @param x x coordinate of the mouse.
   * @param y y coordinate of the mouse.
   */
  private def updateTopMouseOver(x: Double, y: Double): Unit = {
    val prevTop = _topMouseFrame
    _topMouseFrame = topUnderMouse(x, y)

    if (prevTop != _topMouseFrame && prevTop != null) {
      prevTop.fires[Frame, Frame, Unit](ScriptKind.OnLeave)(_topMouseFrame)
      prevTop match {
        case prevTop: Button =>
          prevTop.hideHighlight()
        case _ =>
      }
      _topMouseFrame.fires[Frame, Frame, Unit](ScriptKind.OnEnter)(prevTop)
      _topMouseFrame match {
        case topFrame: Button =>
          if (topFrame.isEnabled) topFrame.showHighlight()
        case _ =>
      }
    }
  }




  /// Handlers

  private var _clickedFrame: Option[Frame] = None
  private var dragFrame: Option[Frame] = None

  /**
   * Does everything that has to be done when a click is performed. This function has to be called inside the
   * mousePressed function of the GameState.
   * It does:
   * - updating top mouse frame, so that we are sure that the right Frame is clicked,
   * - putting the clicked frame on top if button was LeftButton, and if Frame is topLevel enabled,
   * - make button, slider or frame specific actions on click, and
   * - starting a drag procedure, if relevant.
   */
  def clickHandler(mouseX: Double, mouseY: Double, button: Int): Unit = {
    updateTopMouseOver(mouseX, mouseY)

    /// Putting frame on top if top level is enabled
    if (button == 0) {
      if (_topMouseFrame.isTopLevel) _topMouseFrame.putOnTop()
      else _topMouseFrame.ancestors.find(frame => {
        frame.frameStrata == _topMouseFrame.frameStrata && frame.frameLevel == _topMouseFrame.frameLevel &&
        frame.isTopLevel
      }) match {
        case Some(f) => f.putOnTop()
        case _ =>
      }
    }

    _topMouseFrame match {
      case but: Button => if (but.isEnabled) {
        but.fires[Frame, Double, Double, Int, Unit](ScriptKind.OnClick)(mouseX, mouseY, button)
        but.setState(Pushed)
        _clickedFrame = Some(but)
      }
      case slider: Slider => if (slider.isEnabled) Slider.onClickHandler(slider, mouseX, mouseY)
      case frame: Frame => if (_topMouseFrame.isMouseEnabled) {
        frame.fires[Frame, Double, Double, Int, Unit](ScriptKind.OnClick)(mouseX, mouseY, button)
        _clickedFrame = Some(frame)
      }
      case _ =>
    }


    /// Dragging procedure

    if (_topMouseFrame.isRegisteredForDrag && _topMouseFrame.draggingInfo.get.button == button) {
      val referenceFrame = _topMouseFrame.clamp match {
        case Some(f) => f
        case None => UIParent
      }
      _topMouseFrame.draggingInfo.get match {
        case _: FrameDragging =>
          _topMouseFrame.dragEventInfo = Some(new DragEventInfo(
            Complex(mouseX, mouseY),
            mouseX - _topMouseFrame.left + referenceFrame.left,
            mouseY - _topMouseFrame.bottom + referenceFrame.bottom)
          )
          dragFrame = Some(_topMouseFrame)
        case subFrameType: SubFrameDragging =>
          _topMouseFrame.dragEventInfo = Some(new DragEventInfo(
            Complex(mouseX, mouseY),
            mouseX - subFrameType.frame.left + referenceFrame.left,//_topMouseFrame.left + referenceFrame.left,
            mouseY - subFrameType.frame.bottom + referenceFrame.bottom,//_topMouseFrame.bottom + referenceFrame.bottom,
            Some(subFrameType.frame)
          ))
          dragFrame = Some(_topMouseFrame)
        case rectType: RectangularDragging => if (
          mouseX > _topMouseFrame.left + rectType.tlx && mouseX < _topMouseFrame.right + rectType.brx &&
          mouseY > _topMouseFrame.bottom + rectType.bry && mouseY < _topMouseFrame.top + rectType.tly) {
          _topMouseFrame.dragEventInfo = Some(new DragEventInfo(
            Complex(mouseX, mouseY),
            mouseX - _topMouseFrame.left + referenceFrame.left,
            mouseY - _topMouseFrame.bottom + referenceFrame.bottom
          ))
          dragFrame = Some(_topMouseFrame)
        }
        case circularType: CircularDragging => if (math.pow(mouseX - _topMouseFrame.left - circularType.cx, 2) +
        math.pow(mouseY - _topMouseFrame.top - circularType.cy, 2) < math.pow(circularType.radius, 2)) {
          _topMouseFrame.dragEventInfo = Some(new DragEventInfo(
            Complex(mouseX, mouseY),
            mouseX - _topMouseFrame.left + referenceFrame.left,
            mouseY - _topMouseFrame.bottom + referenceFrame.bottom
          ))
          dragFrame = Some(_topMouseFrame)
        }
      }
    }
  }

  /**
   * Does everything that has to be done when the mouse is released. This function has to called in the
   * mouseReleased function of the GameState
   * It does:
   * - update the top frame, so that we are sure that it is applied to the right one,
   * - apply released event on the frame under the mouse,
   * - apply button of frame specific on the frame that was clicked, if it is different from the one under the mouse,
   * and
   * - abort the drag procedure.
   */
  def mouseReleased(mouseX: Double, mouseY: Double, button: Int): Unit = {
    updateTopMouseOver(mouseX, mouseY)

    _topMouseFrame match {
      case but: Button => if (but.isEnabled) {
        but.fires[Frame, Double, Double, Int, Unit](ScriptKind.OnMouseReleased)(mouseX, mouseY, button)
        but.setState(Normal)
      }
      case frame: Frame => if (frame.isMouseEnabled) {
        frame.fires[Frame, Double, Double, Int, Unit](ScriptKind.OnMouseReleased)(mouseX, mouseY, button)
      }
    }

    if (_clickedFrame.isDefined && _clickedFrame.get != _topMouseFrame) _clickedFrame.get match {
      case elem: Button => if (elem.isEnabled) {
        elem.fires[Frame, Double, Double, Int, Unit](ScriptKind.OnMouseReleased)(mouseX, mouseY, button)
        elem.setState(Normal)
      }
      case elem: Frame => if (elem.isMouseEnabled) {
        elem.fires[Frame, Double, Double, Int, Unit](ScriptKind.OnMouseReleased)(mouseX, mouseY, button)
      }
    }

    _clickedFrame = None


    /// Drag procedure (cancel it if it was ongoing)
    if (dragFrame.isDefined) dragFrame = None
    Slider.onMouseReleased(mouseX, mouseY)
  }

  /**
   * Does everything that has to be done when the mouse moves. This function has to be called in mouseMoved
   * function of GameState.
   * It does:
   * - update the frame under the mouse, and triggers any relevant scripts,
   * - move the dragging frame if a procedure is launched, and
   * - do Slider mouse moved specific.
   *
   * @param x      x coordinate of the mouse.
   * @param y      y coordinate of the mouse.
   * @param dx     difference between new x coordinate and previous x coordinate of the mouse.
   * @param dy     difference between new y coordinate and previous y coordinate of the mouse.
   * @param button button given by the JavaScript event.
   */
  def mouseMoved(x: Double, y: Double, dx: Double, dy: Double, button: Int): Unit = {
    updateTopMouseOver(x, y)

    if (dragFrame.isDefined) {
      val f = dragFrame.get
      val drag = f.dragEventInfo.get
      if (drag.alreadyMoved || math.max(math.abs(x - drag.beginPos.re), math.abs(y - drag.beginPos.im)) > 7) {
        drag.alreadyMoved = true
        drag.frame match {
          case Some(g) => moveFrame(g, drag, x, y)
          case None => moveFrame(f, drag, x, y)
        }
      }
    }

    _topMouseFrame.fires[Frame, Double, Double, Double, Double, Int, Unit](ScriptKind.OnMouseMoved)(
      x, y, dx, dy, button
    )

    Slider.onMouseMoved(x, y)
  }


  /**
   * Calls the OnWheelMoved script for relevant frames. Also updates the Frame under the mouse.
   *
   * @param dx 100 from left to right, -100 from right to left, or 0 if no x movement. // to be checked
   * @param dy 100 from top to bottom, -100 from bottom to top, or 0 if no y movement.
   */
  def wheelMoved(dx: Int, dy: Int): Unit = {
    ScriptObject.firesScript[Frame, Int, Int, Unit](ScriptKind.OnWheelMoved)(dx, dy)
    val (x, y) = Engine.mousePosition
    updateTopMouseOver(x,y)
  }


  /// key presses

  private var _keyboardFocus: Option[Focusable] = None

  /** Returns focused [[Focusable]] object, or None if no object has the focus. */
  def keyboardFocus: Option[Focusable] = _keyboardFocus

  /** Sets the new focused [[Focusable]] object. */
  def setKeyboardFocus(frame: Focusable): Unit = _keyboardFocus = Some(frame)

  /** Removes focused [[Focusable]] object. */
  def setKeyboardFocus(): Unit = _keyboardFocus = None

  /**
   * Does everything that has to be done when a keyboard key is pressed. It also returns whether a Frame had focus.
   * Returning this Boolean is important for the Engine to know whether it has to continue processing keyEvent (but this
   * has to be done manually.
   *
   * @param key      JavaScript key name of the pressed key.
   * @param isRepeat whether it is a new press, or it is repeated.
   * @return true whether a frame had focus, false otherwise.
   */
  def keyPressed(key: String, isRepeat: Boolean): Boolean = {
    _keyboardFocus match {
      case Some(frame) =>
        frame.fires[Frame, String, Boolean, Unit](ScriptKind.OnKeyPressed)(key, isRepeat)
        true
      case None =>
        ScriptObject.firesScript[Frame, String, Boolean, Unit](ScriptKind.OnKeyPressed)(key, isRepeat)
        false
    }
  }

  /**
   * Does everything that has to be done when a keyboard key is released. Also returns whether there was a focused
   * object.
   *
   * @param key JavaScript key name of the released key.
   * @return true whether a frame had focus.
   */
  def keyReleased(key: String): Boolean = {
    _keyboardFocus match {
      case Some(frame) =>
        frame.fires[Frame, String, Unit](ScriptKind.OnKeyReleased)(key)
        true
      case None =>
        ScriptObject.firesScript[Frame, String, Unit](ScriptKind.OnKeyReleased)(key)
        false
    }
  }

  /**
   * Calls the OnUpdate script of relevant Frames, and update the [[Tooltip]]s that are appearing or disappearing.
   * @param dt time passed since last called, in milliseconds.
   */
  def updateHandler(dt: Double): Unit = {
    Tooltip.updateTooltips(dt)
    ScriptObject.firesScript[Frame, Double, Unit](ScriptKind.OnUpdate)(dt)
  }



  /// Helper functions

  private def moveFrame(f: Frame, drag: DragEventInfo, x: Double, y: Double): Unit = {
    f.clearAllPoints()

    (f.isDisk, f.clamp) match {
      case (false, Some(g)) =>
        val newX = math.max(drag.xOffset, math.min(x, drag.xOffset - f.width + g.right - g.left))
        val newY = math.max(drag.yOffset, math.min(y, drag.yOffset - f.height + g.top - g.bottom))
        f.setPoint(AnchorPoint(BottomLeft, g, BottomLeft, newX - drag.xOffset, newY - drag.yOffset))
      case (false, None) =>
        f.setPoint(AnchorPoint(BottomLeft, UIParent, BottomLeft, x - drag.xOffset, y - drag.yOffset))
      case (true, Some(g)) =>
        val newX = math.max(drag.xOffset, math.min(x, drag.xOffset - 2 * f.radius + g.right - g.left))
        val newY = math.min(y, drag.yOffset - 2 * f.radius + g.top - g.bottom)
        f.setPoint(AnchorPoint(Center, g, BottomLeft, newX - drag.xOffset + f.radius, newY - drag.yOffset + f.radius))
      case (true, None) =>
        f.setPoint(AnchorPoint(Center, UIParent, BottomLeft, x - drag.xOffset + f.radius, y - drag.yOffset + f.radius))
    }

  }



}