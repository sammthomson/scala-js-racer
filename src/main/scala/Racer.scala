import org.scalajs.dom.{document, window, KeyboardEvent, CanvasRenderingContext2D => Context}
import org.scalajs.dom.html.Canvas
import scala.math.{min, max}
import scalajs.js.JSApp
import collection.{mutable => m}
import scala.util.Random


object Racer extends JSApp with Keys {
  val MillisecondsPerTick = 5

  def main(): Unit = {
    val canvas = document.getElementById("canvas").asInstanceOf[Canvas]
    val screen = Screen(canvas.width, canvas.height)
    val renderer = canvas.getContext("2d").asInstanceOf[Context]
    var state: GameState = GameState.Splash
    window.setInterval(
      () => {
        GameState.render(state, screen)(renderer)
        state = GameState.step(state, keys)
      },
      MillisecondsPerTick
    )
  }
}

// converts from [-1, 1] x [-1, 1] to screen coordinates
case class Screen(w: Int, h: Int) {
  def X(x: Double): Double = w / 2.0 * (1.0 + x)
  def Y(y: Double): Double = h / 2.0 * (1.0 - y)
}

sealed trait GameState
object GameState {
  case object Splash extends GameState {
    val text = Text(
      """Racer
        |
        |Press SPACE to start.
        |LEFT/RIGHT arrow keys to move.
        |SPACE to pause.""".stripMargin,
      x = -0.6,
      y =  0.2,
      color = "red"
    )
  }
  case class Playing(car: Car, road: Road, score: Int) extends GameState
  case class Paused(unpaused: Playing) extends GameState
  object Paused {
    val text = Text(
      """PAUSED.
        |Press SPACE to unpause.""".stripMargin,
      x = -0.5,
      y = 0.0,
      color = "green"
    )
  }
  case object GameOver extends GameState {
    val text = Text(
      s"""GAME OVER.
         |Press SPACE to restart.""".stripMargin,
      x = -0.5,
      y = 0.0,
      color = "red"
    )
  }

  def newGame: GameState = Playing(Car.start, Road.newRoad, 0)

  def render(state: GameState, screen: Screen)(implicit r: Context): Unit = state match {
    case Splash =>
      Splash.text.render(screen)
    case Playing(car, road, score) =>
      r.clearRect(0, 0, screen.w, screen.h)
      Text(s"Score: $score", x = -0.95, y = 0.9, color = "white").render(screen)
      road.render(screen)
      car.render(screen)
    case _: Paused =>
      Paused.text.render(screen)
    case GameOver =>
      GameOver.text.render(screen)
  }

  def step(state: GameState, keys: m.Set[Int]): GameState = {
    import Keys._
    state match {
      case state @ Playing(car, road, score) =>
        val roadSlice = road.xs.head
        if (car.leftBumper < roadSlice.leftWall
            || car.rightBumper > roadSlice.rightWall) {
          // went off the road
          GameOver
        } else {
          // handle arrow key-presses
          if (keys(Space)) {
            keys -= Space
            Paused(state)
          } else {
            val newCar =
              if (keys(Left)) car.updatedLeft
              else if (keys(Right)) car.updatedRight
              else car
            Playing(newCar, road.forward, score + 1)
          }
        }
      case Paused(unpaused) =>
        if (keys(Space)) {
          keys -= Space
          unpaused
        } else state
      case _ =>  // Splash || GameOver
        if (keys(Space)) {
          keys -= Space
          newGame
        } else state
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
    val lines = text.split("\n")
    r.save()
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

  def updatedLeft: Car = Car(x - Car.Agility)
  def updatedRight: Car = Car(x + Car.Agility)

  def render(screen: Screen)(implicit r: Context): Unit = {
    val Bottom = -0.99
    r.fillStyle = Car.Color
    val left = screen.X(leftBumper)
    val leftQrtr = screen.X(x - Car.Width / 4)
    val right = screen.X(rightBumper)
    val rightQrtr = screen.X(x + Car.Width / 4)
    val width = right - left
    val bottom = screen.Y(Bottom)
    val top = screen.Y(Bottom + Car.Height)
    val mid = screen.Y(Bottom + Car.Height / 2)
    val height = bottom - top
    r.fillRect(left, mid, width, height / 2.0)
    r.fillRect(leftQrtr, top, rightQrtr - leftQrtr, height)
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
    r.fillStyle = Road.Color
    for ((slice, i) <- xs.zipWithIndex.take(screen.h)) {
      val y = screen.h - i
      val leftWall = screen.X(slice.leftWall)
      val rightWall = screen.X(slice.rightWall)
      r.fillRect(leftWall, y, rightWall - leftWall, 1)
    }
  }
}
object Road {
  val Color = "gray"
  val StartWidth = 0.25
  val WidthDecayRate = .9998  // road gets progressively narrower
  val Curviness = 0.012

  case class Slice(center: Double, width: Double) {
    def leftWall: Double = center - width / 2
    def rightWall: Double = center + width / 2
    def step: Slice = Slice(
      max(-1.0, min(1.0, center + (Random.nextDouble() - 0.5) * Curviness)),
      width * WidthDecayRate
    )
  }

  def newRoad: Road = Road(Stream.iterate(Slice(0.0, StartWidth))(_.step))
}

// keep track of keyboard keys pressed in a mutable Set
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
