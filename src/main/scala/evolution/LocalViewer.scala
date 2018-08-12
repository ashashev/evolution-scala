package evolution

import scala.annotation.tailrec
import scala.swing.event.{ WindowClosing, WindowOpened }
import swing._

trait Drawer {
  def apply(w: World, scale: (Int, Int), g: Graphics2D): Unit
}

object SimpleDrawer extends Drawer {
  private val creatureColor = new Color(128, 128, 128)
  private val diedColor = new Color(64, 64, 64)

  def apply(world: World, scale: (Int, Int), g: Graphics2D): Unit = {
    world.foreach { (p, c) =>
      val clr = c match {
        case c: Creature if c.alive => creatureColor
        case _                      => diedColor
      }
      g.setColor(clr)
      g.fillRect(p._1 * scale._1, p._2 * scale._2, scale._1, scale._2)
    }

  }
}

object LocalViewer extends SimpleSwingApplication {
  def top = new MainFrame {
    private val bgColor = new Color(0, 0, 0)
    private val sizeOfWorld = (160, 120)

    private var drawer: Drawer = SimpleDrawer

    @volatile
    private var world = World(sizeOfWorld, ((30, 60), Creature()), ((50, 60), Creature()))

    val area = new Component {
      preferredSize = new Dimension(640, 480)

      override def paint(g: Graphics2D): Unit = {
        val scale = (size.width / world.size._1, size.height / world.size._2)

        super.paint(g)
        g.setColor(bgColor)
        g.fillRect(0, 0, size.width, size.height)

        drawer(world, scale, g)
      }

      minimumSize = preferredSize
    }

    private val turnNumber = new Label("Turn: 0")

    private val controls = new BoxPanel(Orientation.Vertical) {
      preferredSize = new Dimension(120, 20)
      contents += turnNumber
      contents += Swing.Glue
    }

    title = "Life"
    contents = new BoxPanel(Orientation.Horizontal) {
      contents += area
      contents += controls
    }

    minimumSize = size

    @volatile
    private var theEnd = false

    private val t = new Thread {
      override def run(): Unit = {

        @tailrec
        def turn(): Unit = if (!theEnd) {
          world = world.turn()
          turnNumber.text = s"Turn: ${world.turnNumber}"
          area.repaint()
          turn()
        }

        turn()
      }
    }

    reactions += {
      case WindowOpened(_) =>
        t.start()
      case WindowClosing(_) =>
        theEnd = true
        t.join()
    }
  }
}
