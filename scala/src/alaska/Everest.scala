package alaska

import swing.ListView.Renderer
import swing._
import swing.event._

/**
 * Graphical user interface for Alaska
 */
object Everest extends SimpleSwingApplication {

  val alaska = new Alaska(new PostfixedPrinterFactory)
  
  val inputField = new TextField {
    columns = 10
  }

  case class FactoryMap(name: String, factory: VisitorFactory)
  val strategies = new ComboBox(List(
      FactoryMap("PostfixedPrinter", new PostfixedPrinterFactory),
      FactoryMap("PrefixedPrinter", new PrefixedPrinterFactory),
      FactoryMap("InfixedPrinter", new InfixedPrinterFactory))) {
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
    alaska.visitorFactory = strategies.selection.item.factory
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
