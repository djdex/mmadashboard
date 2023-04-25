import scalafx.application.*
import scalafx.collections.ObservableBuffer
import scalafx.event
import scalafx.geometry.*
import scalafx.scene.*
import scalafx.geometry.Insets
import scalafx.scene.effect.DropShadow
import scalafx.scene.paint.Color.*
import scalafx.scene.paint.*
import scalafx.scene.text.Text
import scalafx.scene.*
import scalafx.scene.layout.*
import scalafx.scene.control.*
import scalafx.scene.chart.NumberAxis
import scalafx.scene.chart.ScatterChart
import scalafx.scene.chart.XYChart
import scalafx.stage.Stage


object Dashboard extends JFXApp3 {

  var fighterData: FighterData = DataParser.XMLtoFighterData(xml.XML.loadFile("MMA_v2_Competitor_Profile_Example.xml"))


  def test =
    val elem = new GridPane
    val xAxis = new NumberAxis(0, 10, 1)
    val yAxis = new NumberAxis(-100, 500, 100)
    val sc = new ScatterChart[Number, Number](xAxis, yAxis)
    xAxis.setLabel("Age (years)")
    yAxis.setLabel("Returns to date")
    sc.setTitle("Investment Overview")
    val series1 = new XYChart.Series[Number, Number]
    series1.setName("Equities")
    series1.getData.add(new javafx.scene.chart.XYChart.Data(4.2, 193.2))
    series1.getData.add(XYChart.Data(2.8, 33.6))
    series1.getData.add(XYChart.Data(6.2, 24.8))
    series1.getData.add(XYChart.Data(1, 14))
    series1.getData.add(XYChart.Data(1.2, 26.4))
    series1.getData.add(XYChart.Data(4.4, 114.4))
    series1.getData.add(XYChart.Data(8.5, 323))
    series1.getData.add(XYChart.Data(6.9, 289.8))
    series1.getData.add(XYChart.Data(9.9, 287.1))
    series1.getData.add(XYChart.Data(0.9, -9))
    series1.getData.add(XYChart.Data(3.2, 150.8))
    series1.getData.add(XYChart.Data(4.8, 20.8))
    series1.getData.add(XYChart.Data(7.3, -42.3))
    series1.getData.add(XYChart.Data(1.8, 81.4))
    series1.getData.add(XYChart.Data(7.3, 110.3))
    series1.getData.add(XYChart.Data(2.7, 41.2))
    val series2 = new XYChart.Series[Number, Number]
    series2.setName("Mutual funds")
    series2.getData.add(XYChart.Data(5.2, 229.2))
    series2.getData.add(XYChart.Data(2.4, 37.6))
    series2.getData.add(XYChart.Data(3.2, 49.8))
    series2.getData.add(XYChart.Data(1.8, 134))
    series2.getData.add(XYChart.Data(3.2, 236.2))
    series2.getData.add(XYChart.Data(7.4, 114.1))
    series2.getData.add(XYChart.Data(3.5, 323))
    series2.getData.add(XYChart.Data(9.3, 29.9))
    series2.getData.add(XYChart.Data(8.1, 287.4))
    sc.getData.addAll(series1, series2)
    sc.setPrefWidth(400)
    sc.setPrefHeight(400)
    sc.setMinWidth(400)
    sc.setMinHeight(400)
    elem.add(Label("ScatterPlot"), 0, 0)
    elem.add(sc, 0, 1)
    elem



  override def start(): Unit =
    stage = new JFXApp3.PrimaryStage {
      title = "ufcdash"

      val fighterSelectDialog = new Dialog [Unit] () {
        initOwner(stage)
        title = "Fighter Select"
        headerText = "Select weight class and fighter"
        scalafx.Includes.jfxDialogPane2sfx(dialogPane()).buttonTypes = Seq(ButtonType.OK, ButtonType.Cancel)
      }

      val weightClassSelect = new ComboBox(FileHandler.getRankedFighters.map(_._1))
      weightClassSelect.setPromptText("Weight Class")
      val fighterSelect = new ComboBox[String]()
      fighterSelect.setPromptText("Fighter")

      weightClassSelect.onAction = (e: javafx.event.ActionEvent) =>
        fighterSelect.items = scalafx.collections.ObservableBuffer[String]()
        FileHandler.getRankedFighters.find(_._1 == weightClassSelect.value.apply()).getOrElse(("", List[(String, String)](("",""))))._2.foreach((n: (String,String)) => fighterSelect.+=(n._1))

      val fighterSelectGrid = new GridPane() {
        hgap = 12
        vgap = 12
        padding = Insets(20, 100, 10, 10)

        add(weightClassSelect, 0, 0)
        add(fighterSelect, 0, 1)
      }

      scalafx.Includes.jfxDialogPane2sfx(fighterSelectDialog.dialogPane()).content = fighterSelectGrid

      //Selects fighter once user presses OK
      fighterSelectDialog.resultConverter = dialogButton =>
        if (dialogButton == ButtonType.OK) then
          val selectedFighter = fighterSelect.value.apply()

          if selectedFighter.nonEmpty then
            val correctWeightClass = FileHandler.getRankedFighters.find(_._2.exists(_._1 == selectedFighter))
            if correctWeightClass.nonEmpty then
              val fighterNameId = correctWeightClass.get._2.find(_._1 == selectedFighter)
              println(fighterNameId)


      val menuBar = new MenuBar
      val file = new Menu("File")
      val saveItem = new MenuItem("Save")
      val loadItem = new MenuItem("Load")

      file.items = List(saveItem, loadItem)


      val fighters = new Menu("Fighters")
      val fighterSelectItem = new MenuItem("Select Fighter")
      val fighterListItem = new MenuItem("Regenerate Fighter List")

      fighters.items = List(fighterSelectItem, fighterListItem)

      fighterSelectItem.onAction = (e: javafx.event.ActionEvent) =>
        fighterSelectDialog.showAndWait()

      //Handles alert for regenerating fighter list
      val listRegenAlert = new Alert(Alert.AlertType.Confirmation) {
        initOwner(stage)
        title = "Warning"
        headerText = "Are you sure you want to proceed?"
        contentText = "Regenerating the fighter list will produce a new API call.\nNote: This may take a while."
      }

      //If user presses OK, FileHandler will call the API
      fighterListItem.onAction = (e: javafx.event.ActionEvent) =>
        val result = listRegenAlert.showAndWait()

        result match
          case Some(ButtonType.OK) => FileHandler.getFightersAPI()
          case _ =>

      menuBar.menus = List(file, fighters)

      val rootPane = new BorderPane
      rootPane.top = menuBar

      val uiPane = new ScrollPane
      uiPane.fitToWidth = true

      val newElement = new Button("Add UI Element")
      val elements = new HBox(newElement) {
        spacing = 10
        padding = Insets(40, 10, 10, 10)
      }

      uiPane.content = elements

      rootPane.center = uiPane

      rootPane.prefWidth = (800)
      rootPane.prefHeight = (500)

      scene = new Scene {
        root = rootPane
      }
      newElement.onAction = (e: javafx.event.ActionEvent) => {
        elements.children.add(elements.children.size - 1, new UIElementParent)
      }
    }


}



