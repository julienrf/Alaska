package alaska

import swing.ListView.Renderer
import swing._
import swing.event._

/**
 * Graphical user interface for Alaska
 */
object Everest extends SimpleSwingApplication {

  val alaska = new Alaska(new PostfixedPrinter with Logger)
  
  val inputField = new TextField {
    columns = 10
  }

  case class FactoryMap(name: String, visitor: ExprVisitor)
  val strategies = new ComboBox(List(
      FactoryMap("PostfixedPrinter", new PostfixedPrinter with Logger),
      FactoryMap("PrefixedPrinter", new PrefixedPrinter with Logger),
      FactoryMap("InfixedPrinter", new InfixedPrinter with Logger))) {
      renderer = Renderer(_.name)
  }
  val resultLbl = new Label {
    text = "Write an arithmetic expression"
  }

  listenTo(strategies.selection, inputField)
  reactions += {
    case EditDone(`inputField`) => updateResult()
    case SelectionChanged(`strategies`) => changeStrategy()
  }

  def updateResult(): Unit = {
    try {
      resultLbl.text = alaska process ExprParser(inputField.text)
    } catch {
      case _ => resultLbl.text = "Write an arithmetic expression"
    }
  }

  def changeStrategy(): Unit = {
    alaska.visitor = strategies.selection.item.visitor
    updateResult()
  }

  override def top = new MainFrame {
    title = "Alaska"
    contents = new BoxPanel(Orientation.Vertical) {
      contents += new FlowPanel(inputField)
      contents += new FlowPanel(strategies)
      contents += new FlowPanel(resultLbl)
    }
  }
}
