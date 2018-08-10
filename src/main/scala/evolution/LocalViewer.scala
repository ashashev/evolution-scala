package evolution

import scala.annotation.tailrec
import scala.swing.event.{ WindowClosing, WindowOpened }
import swing._

object LocalViewer extends SimpleSwingApplication {
  def top = new MainFrame {
    val bgColor = new Color(0, 0, 0)
    val creatureColor = new Color(128, 128, 128)
    @volatile
    var world = World((160, 120), ((30, 30), Creature()), ((50, 50), Creature()))
    val area = new Component {
      preferredSize = new Dimension(640, 480)

      override def paint(g: Graphics2D): Unit = {
        val scale = (size.width / world.size._1, size.height / world.size._2)

        super.paint(g)
        g.setBackground(bgColor)
        g.fillRect(0, 0, size.width, size.height)
        g.setColor(creatureColor)

        world.foreach((p, _) =>
          g.fillRect(p._1 * scale._1, p._2 * scale._2, scale._1, scale._2)
        )
      }

      minimumSize = preferredSize
    }

    val turnNumber = new Label("Turn: 0")

    val controls = new BoxPanel(Orientation.Vertical) {
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

    val t = new Thread {
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
