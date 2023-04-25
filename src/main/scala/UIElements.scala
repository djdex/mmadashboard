import scalafx.scene.*
import scalafx.scene.chart.*
import scalafx.scene.layout.*
import scalafx.scene.control.*

class UIElementParent extends BorderPane {
  protected var childElement: UIElement = new StatCard {update}

  val elementSelect = new ComboBox[String](List("Scatter Plot", "MultiSeries Plot", "Pie Chart", "Bar Chart", "Stat Card"))
  top = elementSelect

  this.minWidth = 100

  elementSelect.onAction = (e: javafx.event.ActionEvent) =>
    elementSelect.value.apply() match
      case "Scatter Plot" => setChild(new ScatterPlot)
      case "MultiSeries Plot" =>
      case "Stat Card" => setChild(new StatCard)

  def setChild(element: UIElement) =
    center = element
    childElement = element
    childElement.update

  def setChildData(data: FighterData) =
    childElement.fighterInfo = data
    childElement.update
}

abstract class UIElement extends GridPane {

  var fighterInfo: FighterData = DataParser.XMLtoFighterData(xml.XML.loadFile("MMA_v2_Competitor_Profile_Example.xml"))

  def getField(field: String, dataPoint: FightData): Double

  def update: Unit
}

class StatCard extends UIElement {

  val text = new Label("Name: " + fighterInfo.name)

  override def getField(field: String, dataPoint: FightData): Double = ???

  def update =
    text.text = "Name: " + fighterInfo.name
    text.minWidth = 100
    text.minHeight = 100
    add(text, 0, 0)
}

class ScatterPlot extends UIElement {

  val isPerMin = new CheckBox() {
    this.text = "Is per minute"

    this.onAction = (e: javafx.event.ActionEvent) =>
      update
  }

  val fieldSelect1 = new ComboBox[String](
    List("Strikes Attempted", "Strikes Landed", "Strike Percentage",
      "Significant Strikes Attempted", "Significant Strikes Landed", "Significant Strike Percentage",
      "Takedowns Attempted", "Takedown Percentage")) {
    this.getSelectionModel.select(1)

    this.onAction = (e: javafx.event.ActionEvent) => update
  }
  val fieldSelect2 = new ComboBox[String](
    List("Strikes Attempted", "Strikes Landed",
      "Significant Strikes Attempted", "Significant Strikes Landed",
      "Takedowns Attempted")) {
    this.getSelectionModel.select(1)

    this.onAction = (e: javafx.event.ActionEvent) => update
  }
  var currentField = ""

  def comboBox =
    if isPerMin.selected.value then
      fieldSelect2
    else
      fieldSelect1

  override def getField(field: String, dataPoint: FightData): Double = ???

  def update =

    val maxVal = fighterInfo.fights.map(_.getField(comboBox.value.apply(), false)).maxOption.getOrElse(10.0)

    val maxDate = fighterInfo.fights.map(_.dateNum).maxOption.getOrElse(2023.0).toInt
    val minDate = fighterInfo.fights.map(_.dateNum).minOption.getOrElse(2015.0).toInt

    val xAxis = new NumberAxis(minDate - 1, maxDate + 1, 1)
    val yAxis = new NumberAxis(-maxVal * 0.2, maxVal * 1.2, maxVal / 10)
    val sc = new ScatterChart[Number, Number](xAxis, yAxis)

    val series =  new XYChart.Series[Number, Number]
    series.setName(comboBox.value.apply())
    for point <- this.fighterInfo.fights do
      println(point.getField(comboBox.value.apply(), false))
      println(point.dateNum)
      series.getData.add(XYChart.Data(point.dateNum, point.getField(comboBox.value.apply(), false)))
    end for

    sc.getData.add(series)

    sc.setPrefWidth(400)
    sc.setPrefHeight(400)
    sc.setMinWidth(400)
    sc.setMinHeight(400)

    this.children.clear()
    this.add(sc, 1, 1, 2, 2)
    this.add(comboBox, 1, 4)
    this.add(isPerMin, 2, 4)

}