package evolution

import scala.annotation.tailrec
import scala.swing.event.{ WindowClosing, WindowOpened, SelectionChanged }
import swing._
import java.awt.Color

trait Drawer {
  def apply(w: World, scale: (Int, Int), g: Graphics2D): Unit = {
    w.foreach { (p, c) =>
      g.setColor(getColor(c))
      g.fillRect(p._1 * scale._1, p._2 * scale._2, scale._1, scale._2)
    }
  }
  def getColor(c: Cell): Color
}

object Drawer {
  val drawers = Map(
    "Simple" -> SimpleDrawer,
    "Enregy Levels" -> EnergyLevels)
}

object SimpleDrawer extends Drawer {
  private val creatureColor = new Color(128, 128, 128)
  private val diedColor = new Color(64, 64, 64)

  def getColor(c: Cell): Color = c match {
    case c: Creature if c.alive => creatureColor
    case _                      => diedColor
  }
}

object EnergyLevels extends Drawer {
  private val aliveHueRange = (100.0f, 0.0f)
  private val diedHueRange = (200.0f, 300.0f)

  private val saturation = 1.0f
  private val brightness = 1.0f

  private val aliveRangeSize = (aliveHueRange._2 - aliveHueRange._1).abs.toFloat
  private val diedRangeSize = (diedHueRange._2 - diedHueRange._1).abs.toFloat

  def getColor(c: Cell): Color = c match {
    case c: Creature if c.alive =>
      val h = aliveHueRange._1 - (c.energy * aliveRangeSize / Creature.maxLevel)
      Color.getHSBColor(h, saturation, brightness)
    case c: Creature =>
      val h = diedHueRange._1 + (c.energy * diedRangeSize / Creature.maxLevel)
      Color.getHSBColor(h, saturation, brightness)
  }
}

object LocalViewer extends SimpleSwingApplication {
  def top = new MainFrame {
    private val bgColor = new Color(0, 0, 0)
    private val sizeOfWorld = (160, 120)

    private def restrictHeight[T <: Component](s: T): T = {
      s.maximumSize = new Dimension(Short.MaxValue, s.preferredSize.height)
      s
    }

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

    private val turnNumber = restrictHeight(new Label("0"))
    turnNumber.preferredSize = new Dimension(
      150, turnNumber.preferredSize.height)
    turnNumber.horizontalAlignment = Alignment.Left

    private val drawers = restrictHeight(new ComboBox(
      Drawer.drawers.keys.toSeq))

    private var drawer: Drawer = Drawer.drawers(drawers.selection.item)

    private val controls = new BoxPanel(Orientation.Vertical) {
      contents += restrictHeight(new BoxPanel(Orientation.Horizontal) {
        val l = new Label("Turn: ")
        l.horizontalAlignment = Alignment.Left
        contents += Swing.HStrut(3)
        contents += l
        contents += Swing.HStrut(5)
        contents += turnNumber
      })

      contents += Swing.VStrut(5)
      contents += drawers
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
          turnNumber.text = s"${world.turnNumber}"
          area.repaint()
          turn()
        }

        turn()
      }
    }

    listenTo(drawers.selection)

    reactions += {
      case WindowOpened(_) =>
        t.start()
      case WindowClosing(_) =>
        theEnd = true
        t.join()
      case SelectionChanged(`drawers`) =>
        drawer = Drawer.drawers(drawers.selection.item)
        area.repaint()
    }
  }
}
