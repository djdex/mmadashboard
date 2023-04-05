import scala.collection.mutable
import scala.swing.*
import scala.swing.GridBagPanel.*
import scala.collection.mutable.Buffer
import scala.swing.TabbedPane.Page

object DashboardGUI extends SimpleSwingApplication {

  val uiElements: Buffer[Component] = Buffer()
  var fighterData: Option[FighterData] = None

  def ua: GridBagPanel = new GridBagPanel {
    def constraints(x: Int, y: Int,
        gridwidth: Int = 1, gridheight: Int = 1,
        weightx: Double = 0.0, weighty: Double = 0.0,
        fill: GridBagPanel.Fill.Value = GridBagPanel.Fill.None) : Constraints = {
      val c = new Constraints
      c.gridx = x
      c.gridy = y
      c.gridwidth = gridwidth
      c.gridheight = gridheight
      c.weightx = weightx
      c.weighty = weighty
      c.fill = fill
      c
    }

    val nameText = new Label("Name: " + fighterData.getOrElse(new FighterData).name)
    val weightText = new Label("Weight: " + fighterData.getOrElse(new FighterData).weight.toString)

    add(nameText, constraints(0, 0))
    add(new Button(Action("Hi"){weightText.text = "Weight: " + fighterData.getOrElse(new FighterData).weight.toString; revalidate(); repaint()}), constraints(1, 0))
    add(weightText, constraints(0, 1))
    add(new Button("Hello"), constraints(1, 1))

  }

  val ui: GridBagPanel = new GridBagPanel {

    def constraints(x: Int, y: Int,
        gridwidth: Int = 1, gridheight: Int = 1,
        weightx: Double = 0.0, weighty: Double = 0.0,
        fill: GridBagPanel.Fill.Value = GridBagPanel.Fill.None) : Constraints = {
      val c = new Constraints
      c.gridx = x
      c.gridy = y
      c.gridwidth = gridwidth
      c.gridheight = gridheight
      c.weightx = weightx
      c.weighty = weighty
      c.fill = fill
      c
    }
    add(new Button(Action("newthing"){add(ua,constraints(1,0))}), constraints(0,0))
  }

  val test: BoxPanel = new BoxPanel(Orientation.Horizontal) {
    contents += new Button(Action("newthing"){
      contents += ua
      revalidate()
      repaint()
      println(contents)
    })
    border = Swing.LineBorder(java.awt.Color.BLACK)
  }

  val mainMenu: BoxPanel = new BoxPanel(Orientation.Horizontal) {
    contents += new Button(Action("Load Stats") {
      fighterData = Some(DataParser.XMLtoFighterData(xml.XML.loadFile("MMA_v2_Competitor_Profile_Example.xml")))
    })
  }

  var selected = mainMenu

  def top: Frame = new MainFrame {
    title = "ufcdash"

    preferredSize = new Dimension(500, 200)
    centerOnScreen()

    menuBar = new MenuBar{
      contents += new Menu("File") {
        contents += new MenuItem(Action("Save Current Dashboard") {
          println("saved")
          test.contents -= test.contents.last
          test.repaint()
        })
        contents += new MenuItem(Action("Load Fighter From File") {
          println("loaded")
        })
      }
    }

    contents = new TabbedPane {
      pages += new Page("Menu", mainMenu)
      pages += new Page("test", test)

    }

  }
}
