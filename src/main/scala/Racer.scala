import org.scalajs.dom.html.Canvas
import org.scalajs.dom.{CanvasRenderingContext2D => Context, KeyboardEvent, MouseEvent, document, window}

import scala.collection.{mutable => m}
import scala.math.{max, min}
import scala.scalajs.js.JSApp
import scala.util.Random


class Racer(val canvas: Canvas) extends Keys with Mouse {
  val screen = Screen(canvas.width, canvas.height)
  val r = canvas.getContext("2d").asInstanceOf[Context]

  var state: GameState = GameState.Splash

  def run(): Unit = window.setInterval(
    () => {
      GameState.render(state, screen)(r)
      state = step(state)
    },
    Racer.MillisecondsPerTick
  )

  def step(state: GameState): GameState = {
    import Keys._
    state match {
      case state @ GameState.Playing(car, road, score) =>
        val roadSlice = road.xs.head
        if (car.leftBumper < roadSlice.leftWall
            || car.rightBumper > roadSlice.rightWall) {
          // went off the road
          mouse = None
          GameState.GameOver(state)
        } else {
          // handle arrow key-presses
          if (keys(Space)) {
            keys -= Space
            GameState.Paused(state)
          } else {
            // move car
            var newCar = car
            if (keys(Left))
              newCar = newCar.updated(_ - Car.Agility)
            if (keys(Right))
              newCar = newCar.updated(_ + Car.Agility)
            mouse.foreach { case (mouseX, _) =>
              newCar = newCar.updated(carX => carX + (mouseX - carX) * 2 * Car.Agility)
            }
            GameState.Playing(newCar, road.forward, score + 1)
          }
        }
      case GameState.Paused(unpaused) =>
        if (keys(Space) || mouse.isDefined) {
          keys -= Space
          unpaused
        } else state
      case _ =>  // Splash || GameOver
        if (keys(Space) || mouse.isDefined) {
          keys -= Space
          GameState.newGame
        } else state
    }
  }
}
object Racer extends JSApp {
  val MillisecondsPerTick = 5

  def main(): Unit = {
    val canvas = document.getElementById("canvas").asInstanceOf[Canvas]
    new Racer(canvas).run()
  }
}

// converts from [-1, 1] x [-1, 1] to screen coordinates
case class Screen(w: Int, h: Int) {
  def X(x: Double): Double = w / 2.0 * (1.0 + x)
  def Y(y: Double): Double = h / 2.0 * (1.0 - y)
}

// keep track of mouse state
trait Mouse {
  def canvas: Canvas
  def screen: Screen
  var mouse: Option[(Double, Double)] = None
  def userSpace(x: Double, y: Double): (Double, Double) = (2 * x / screen.w - 1, 2 * y / screen.h - 1)
  canvas.onmousedown = (e: MouseEvent) => { mouse = Some(userSpace(e.clientX, e.clientY)) }
  canvas.onmousemove = (e: MouseEvent) => { if (mouse.isDefined) mouse = Some(userSpace(e.clientX, e.clientY)) }
  canvas.onmouseup = (e: MouseEvent) => { mouse = None }
}
// keep track of keyboard keys pressed
trait Keys {
  lazy val keys = m.Set.empty[Int]
  window.onkeydown = (e: KeyboardEvent) => keys += e.keyCode
  window.onkeyup = (e: KeyboardEvent) => keys -= e.keyCode
}
object Keys {
  val Space = 32
  val Left = 37
  val Up = 38
  val Right = 39
  val Down = 40
}


sealed trait GameState
object GameState {
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
      color = "red"
    )
  }
  case class Playing(car: Car, road: Road, score: Int) extends GameState
  case class Paused(unpaused: Playing) extends GameState
  object Paused {
    val text = Text(
      """PAUSED.
        |
        |Press SPACE to unpause.""".stripMargin,
      x = -0.5,
      y = 0.2,
      color = "green"
    )
  }
  case class GameOver(lastState: Playing) extends GameState
  object GameOver {
    val text = Text(
      s"""GAME OVER.
         |
         |Press SPACE to restart.""".stripMargin,
      x = -0.5,
      y = 0.2,
      color = "red"
    )
  }

  def newGame: GameState = Playing(Car.start, Road.newRoad, 0)

  def render(state: GameState, screen: Screen)(implicit r: Context): Unit = {
    r.clearRect(0, 0, screen.w, screen.h)
    state match {
      case Splash =>
        Splash.text.render(screen)
      case Playing(car, road, score) =>
        Text(s"SCORE: $score", x = -0.95, y = 0.85, color = "green").render(screen)
        road.render(screen)
        car.render(screen)
      case Paused(unpaused) =>
        r.save()
        r.globalAlpha = .5
        render(unpaused, screen)
        r.restore()
        Paused.text.render(screen)
      case GameOver(lastState) =>
        r.save()
        r.globalAlpha = .5
        render(lastState, screen)
        r.restore()
        GameOver.text.render(screen)
    }
  }
}


case class Text(text: String,
                x: Double,
                y: Double,
                color: String = "#000000",
                font: String = "'Georgia'",
                size: Int = 30,
                rotation: Double = 0.0) {
  def render(screen: Screen)(implicit r: Context) {
    r.save()
    val lines = text.split("\n")
    r.font = size.toString + "px " + font
    r.fillStyle = color
    r.translate(screen.X(x), screen.Y(y))
    r.rotate(rotation * Math.PI / 180)
    for ((line, i) <- lines.zipWithIndex) {
      r.fillText(line, 0, i * size)
    }
    r.restore()
  }
}


case class Car(x: Double) {
  def leftBumper: Double = x - Car.Width / 2
  def rightBumper: Double = x + Car.Width / 2

  def updated(f: Double => Double): Car = Car(f(x))

  def render(screen: Screen)(implicit r: Context): Unit = {
    r.save()
    val Bottom = -0.99
    r.fillStyle = Car.Color
    val left = screen.X(leftBumper)
    val leftQuarter = screen.X(x - Car.Width / 4)
    val right = screen.X(rightBumper)
    val rightQuarter = screen.X(x + Car.Width / 4)
    val width = right - left
    val bottom = screen.Y(Bottom)
    val top = screen.Y(Bottom + Car.Height)
    val mid = screen.Y(Bottom + Car.Height / 2)
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

  def render(screen: Screen)(implicit r: Context): Unit = {
    r.save()
    r.fillStyle = Road.Color
    for ((slice, i) <- xs.zipWithIndex.take(screen.h)) {
      val y = screen.h - i
      val leftWall = screen.X(slice.leftWall)
      val rightWall = screen.X(slice.rightWall)
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
