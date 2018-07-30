package ashashev

import scala.annotation.tailrec
import scala.swing.event.{WindowClosing, WindowOpened}
import swing._

object LocalViewer extends SimpleSwingApplication {
  def top = new MainFrame {
    @volatile
    private var creatures = List.empty[Creature]
    val bgColor = new Color(0, 0, 0)
    val creatureColor = new Color(128, 128, 128)
    val world = World((160, 120))
    val area = new Component {
      preferredSize = new Dimension(640, 480)

      override def paint(g: Graphics2D): Unit = {
        val scale = (size.width / world.size._1, size.height / world.size._2)

        super.paint(g)
        g.setBackground(bgColor)
        g.fillRect(0, 0, size.width, size.height)
        g.setColor(creatureColor)
        creatures.foreach(c => {
          g.fillRect(c.position._1 * scale._1, c.position._2 * scale._2, scale._1, scale._2)
        })
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
        def turn(cs: List[Creature], n: Int): Unit = if (!theEnd) {
          val csn = world.turn(cs)
          turnNumber.text = s"Turn: $n"
          creatures = csn
          area.repaint()
          turn(csn, n + 1)
        }

        turn(List(Creature(30, 30), Creature(50, 50)), 0)
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
