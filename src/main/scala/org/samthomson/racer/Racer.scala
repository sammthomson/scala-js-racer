package org.samthomson.racer

import org.samthomson.pointerevents.{PointerEvent, PointerEventTarget}
import org.scalajs.dom.ext.Color
import org.scalajs.dom.ext.KeyCode.{Left, Right, Space}
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.{Window, UIEvent}
import org.scalajs.dom.{CanvasRenderingContext2D => Context, KeyboardEvent, document, window}

import scala.collection.{mutable => m}
import scala.math.{max, min}
import scala.scalajs.js.JSApp
import scala.util.Random


object Racer extends JSApp {
  val MillisecondsPerTick = 5

  def main(): Unit = {
    val canvas = document.getElementById("canvas").asInstanceOf[Canvas]
    new Racer(canvas).run()
  }
}
class Racer(val canvas: Canvas) {
  def run(): Unit = {
    val screen = GameScreen(canvas)
    screen.resize(window)
    window.onresize = (_: UIEvent) => screen.resize(window)

    val controls = Controls(screen)

    var state = GameState.splash

    window.setInterval(
      () => {
        screen.clear()
        state.render(screen)
        state = state.step(controls)
      },
      Racer.MillisecondsPerTick
    )
  }
}

// converts from [-1, 1] x [-1, 1] to screen coordinates
case class GameScreen(canvas: Canvas) {
  val context = canvas.getContext("2d").asInstanceOf[Context]

  def width = canvas.width
  def height = canvas.height
  def left = canvas.getBoundingClientRect().left
  def top = canvas.getBoundingClientRect().top

  def clear(): Unit = context.clearRect(0, 0, width, height)

  def fromGameSpaceX(gameX: Double): Double = width / 2.0 * (1.0 + gameX)
  def fromGameSpaceY(gameY: Double): Double = height / 2.0 * (1.0 - gameY)
  def fromGameSpace(gameX: Double, gameY: Double) =
    (fromGameSpaceX(gameX), fromGameSpaceY(gameY))

  def toGameSpaceX(clientX: Double): Double =  2 * (clientX - left) / width - 1
  def toGameSpaceY(clientY: Double): Double = -2 * (clientY - top) / height - 1
  def toGameSpace(clientX: Double, clientY: Double): (Double, Double) =
    (toGameSpaceX(clientX), toGameSpaceY(clientY))

  def resize(window: Window) {
    val aspectRatio = 4.0 / 3.0
    val width = window.innerWidth
    val height = window.innerHeight - 30  // leave enough room for link underneath
    if (width > height * aspectRatio) {
      canvas.width = (height * aspectRatio).toInt
      canvas.height = height
    } else {
      canvas.width = width
      canvas.height = (width / aspectRatio).toInt
    }
  }
}

// keep track of pointer state
trait Pointer {
  def screen: GameScreen
  var pointer: Option[(Double, Double)] = None

  screen.canvas.onPointerDown { (e: PointerEvent) =>
    pointer = Some(screen.toGameSpace(e.clientX, e.clientY))
  }
  screen.canvas.onPointerMove { (e: PointerEvent) =>
    if (pointer.isDefined) pointer = Some(screen.toGameSpace(e.clientX, e.clientY))
  }
  screen.canvas.onPointerUp { (e: PointerEvent) => pointer = None }
}
// keep track of keyboard keys pressed
trait Keys {
  val keys = m.Set.empty[Int]

  window.onkeydown = (e: KeyboardEvent) => keys += e.keyCode
  window.onkeyup = (e: KeyboardEvent) => keys -= e.keyCode
}
case class Controls(screen: GameScreen) extends Pointer with Keys


sealed trait GameState {
  def render(screen: GameScreen): Unit
  def step(controls: Controls): GameState
}
object GameState {
  def splash: GameState = Splash
  def newGame: GameState = Playing(Car.start, Road.newRoad, 0)

  def ifClick(controls: Controls, next: => GameState, current: => GameState): GameState =
    if (controls.keys(Space) || controls.pointer.isDefined) {
      controls.keys -= Space
      next
    } else {
      current
    }
}
case object Splash extends GameState {
  val text = Text(
    """Racer
      |
      |Press SPACE or touch to start.
      |
      |Use arrow keys or touch to move.
      |
      |Press SPACE to pause.""".stripMargin,
    x = -0.6,
    y =  0.4,
    color = Color.Red
  )

  override def render(screen: GameScreen): Unit = text.render(screen)

  override def step(controls: Controls): GameState =
    GameState.ifClick(controls, GameState.newGame, this)
}
case class Playing(car: Car, road: Road, score: Int) extends GameState {
  override def render(screen: GameScreen): Unit = {
    road.render(screen)
    scoreText.render(screen)
    car.render(screen)
  }

  def scoreText: Text = Text(s"SCORE: $score", x = -0.95, y = 0.85, color = Color.Green)

  override def step(controls: Controls): GameState = {
    val roadSlice = road.xs.head
    if (car.leftBumper < roadSlice.leftWall
        || car.rightBumper > roadSlice.rightWall) {
      // went off the road
      controls.pointer = None
      GameOver(this)
    } else {
      // handle arrow key-presses
      if (controls.keys(Space)) {
        controls.keys -= Space
        Paused(this)
      } else {
        // move car
        var newCar = car
        if (controls.keys(Left))
          newCar = newCar.updated(_ - Car.Agility)
        if (controls.keys(Right))
          newCar = newCar.updated(_ + Car.Agility)
        controls.pointer.foreach { case (mouseX, _) =>
          newCar = newCar.updated(carX => carX + (mouseX - carX) * 2 * Car.Agility)
        }
        Playing(newCar, road.forward, score + 1)
      }
    }
  }
}
case class Paused(unpaused: Playing) extends GameState {
  override def render(screen: GameScreen): Unit = {
    val r = screen.context
    r.save()
    r.globalAlpha = .5
    unpaused.render(screen)
    r.restore()
    Paused.text.render(screen)
  }

  override def step(controls: Controls): GameState =
    GameState.ifClick(controls, unpaused, this)
}
object Paused {
  val text = Text(
    """PAUSED.
      |
      |Press SPACE to unpause.""".stripMargin,
    x = -0.5,
    y = 0.2,
    color = Color.Green
  )
}
case class GameOver(lastState: Playing) extends GameState {
  override def render(screen: GameScreen): Unit = {
    val r = screen.context
    r.save()
    r.globalAlpha = .5
    lastState.render(screen)
    r.restore()
    GameOver.text.render(screen)
  }

  override def step(controls: Controls): GameState =
    GameState.ifClick(controls, GameState.newGame, this)
}
object GameOver {
  val text = Text(
    s"""GAME OVER.
        |
        |Press SPACE to restart.""".stripMargin,
    x = -0.5,
    y = 0.2,
    color = Color.Red
  )
}


case class Text(text: String,
                x: Double,
                y: Double,
                color: Color = Color.White,
                font: String = "'Georgia'",
                size: Int = 30,
                rotation: Double = 0.0) {
  def render(screen: GameScreen) {
    val r = screen.context
    r.save()
    r.font = size.toString + "px " + font
    r.fillStyle = color.toString
    r.translate(screen.fromGameSpaceX(x), screen.fromGameSpaceY(y))
    r.rotate(rotation * Math.PI / 180)
    for ((line, i) <- text.split("\n").zipWithIndex) {
      r.fillText(line, 0, i * size)
    }
    r.restore()
  }
}


case class Car(x: Double) {
  def leftBumper: Double = x - Car.Width / 2
  def rightBumper: Double = x + Car.Width / 2

  def updated(f: Double => Double): Car = Car(f(x))

  def render(screen: GameScreen): Unit = {
    val r = screen.context
    r.save()
    val Bottom = -0.99
    r.fillStyle = Car.Color
    val left = screen.fromGameSpaceX(leftBumper)
    val leftQuarter = screen.fromGameSpaceX(x - Car.Width / 4)
    val right = screen.fromGameSpaceX(rightBumper)
    val rightQuarter = screen.fromGameSpaceX(x + Car.Width / 4)
    val width = right - left
    val bottom = screen.fromGameSpaceY(Bottom)
    val top = screen.fromGameSpaceY(Bottom + Car.Height)
    val mid = screen.fromGameSpaceY(Bottom + Car.Height / 2)
    val height = bottom - top
    r.fillRect(left, mid, width, height / 2.0)
    r.fillRect(leftQuarter, top, rightQuarter - leftQuarter, height)
    r.restore()
  }
}
object Car {
  val Color = "red"
  val Width = 0.03
  val Height = 0.03
  val Agility = Width / 25

  def start: Car = Car(0.0)
}

case class Road(xs: Stream[Road.Slice]) {
  def forward: Road = Road(xs.tail)

  def render(screen: GameScreen): Unit = {
    val r = screen.context
    r.save()
    r.fillStyle = Road.Color
    for ((slice, i) <- xs.zipWithIndex.take(screen.height)) {
      val y = screen.height - i
      val leftWall = screen.fromGameSpaceX(slice.leftWall)
      val rightWall = screen.fromGameSpaceX(slice.rightWall)
      r.fillRect(leftWall, y, rightWall - leftWall, 1)
    }
    r.restore()
  }
}
object Road {
  val Color = "gray"
  val StartWidth = 0.25
  val WidthDecayRate = .9998  // road gets narrower each step
  val Curviness = 0.012

  case class Slice(center: Double, width: Double) {
    def leftWall: Double = center - width / 2
    def rightWall: Double = center + width / 2
    def step: Slice = Slice(
      max(-1, min(1, center + (Random.nextDouble() - 0.5) * Curviness)),
      width * WidthDecayRate
    )
  }

  def newRoad: Road = Road(Stream.iterate(Slice(0.0, StartWidth))(_.step))
}
