import javafx.event.ActionEvent
import scalafx.scene.*
import scalafx.scene.chart.{PieChart, *}
import scalafx.scene.layout.*
import scalafx.scene.control.*
import scalafx.scene.paint.*
import scalafx.collections.CollectionIncludes.observableList2ObservableBuffer
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.shape.*

import java.util.Calendar
import scala.collection.mutable.Buffer

class DefaultElement extends StackPane {

  val size = 300

  val gridPane = new GridPane {
    this.padding = Insets(30, 10, 10, 20)
  }

  val fieldText = new Label("Name: \n\nNationality: \n\nWins: \n\nLosses: \n\nDraws: ") {
    font.value = new scalafx.scene.text.Font(14)
  }

  val valueText = new Label {
    font.value = new scalafx.scene.text.Font(14)
    alignment = Pos.CenterRight
  }

  val rectangle = new Rectangle {
    width = 280
    height = 220
    stroke.value = Color.Black
    fill.value = Color.Transparent
  }

  gridPane.add(fieldText, 0, 0)
  gridPane.add(valueText, 0, 1)

  this.children.add(gridPane)
  this.children.add(rectangle)


  def update(fighterInfo: FighterData) =
    this.children.clear()
    gridPane.children.clear()

    valueText.text = s"${fighterInfo.name}\n\n${fighterInfo.nationality}\n\n${fighterInfo.wins}\n\n${fighterInfo.losses}\n\n${fighterInfo.draws}"

    gridPane.add(fieldText, 0, 0)
    gridPane.add(valueText, 1, 0)

    this.children.add(rectangle)
    this.children.add(gridPane)

}

//The UIElementParent class is the container for the different UI elements
class UIElementParent(val deleteDuplicate: ((UIElementParent, Boolean) => Unit)) extends BorderPane {
  var fighterData = new FighterData

  protected var childElement: UIElement = new ScatterPlot(this) {update}

  val elementSelect = new ComboBox[String](List("Scatter Plot", "MultiSeries Plot", "Pie Chart", "Bar Chart", "Stat Card")) {this.getSelectionModel.select(0)}
  top = elementSelect

  setChild(new ScatterPlot(this))

  elementSelect.onAction = (e: javafx.event.ActionEvent) =>
    elementSelect.value.apply() match
      case "Scatter Plot" => setChild(new ScatterPlot(this))
      case "MultiSeries Plot" => setChild(new MultiScatterPlot(this))
      case "Bar Chart" => setChild(new BarChart(this))
      case "Stat Card" => setChild(new StatCard(this))
      case "Pie Chart" => setChild(new PieChart(this))

  def getChild =
    childElement

  def setChild(element: UIElement) =
    center = element
    childElement = element
    childElement.update

}

abstract class UIElement (parent: UIElementParent) extends GridPane {

  def fighterInfo: FighterData = parent.fighterData

  var maxSize = 1000
  var minSize = 250

  var size = 400
  val paddingX: Int
  val paddingY: Int

  val sizeplusItem = new MenuItem("Increase Size") {
    onAction = (e: ActionEvent) =>
      size = math.min(maxSize, size + 50)
      update
  }
  val sizeminusItem = new MenuItem("Decrease Size") {
    onAction = (e: ActionEvent) =>
      size = math.max(minSize, size - 50)
      update
  }
  val duplicateItem = new MenuItem("Duplicate") {
    onAction = (e: ActionEvent) =>
      parent.deleteDuplicate(parent, false)
  }
  val deleteItem = new MenuItem("Delete") {
    onAction = (e: ActionEvent) =>
      parent.deleteDuplicate(parent, true)
  }
  val settings = new Menu("Settings") {
    items = List(sizeplusItem, sizeminusItem, duplicateItem, deleteItem)
  }
  val menuBar = new MenuBar() {
    this.setMinWidth(80)
    this.setMaxWidth(80)
    menus = List(settings)
  }

  def update: Unit
}

//This class handles selection from a list of multiple fields
class FieldSelect(field: String, parentElem: UIElement) {

  val fieldName = new Label(field)

  val isSelected = new CheckBox() {
    this.onAction = (e: ActionEvent) =>
      parentElem.update
  }

  val colourSelect: ComboBox[String] = new ComboBox(List("Red", "Black", "Gray", "Purple", "Green", "Blue", "Yellow", "Orange")) {
    this.getSelectionModel.select(0)

    this.onAction = (e: ActionEvent) =>
      parentElem.update
  }
}

class StatCard (parent: UIElementParent) extends UIElement (parent) {

  override val paddingX: Int = 0
  override val paddingY: Int = 0

  minSize = 100
  maxSize = 500

  size = 200

  val operationSelect = new ComboBox[String](List(
    "Career Total",
    "Maximum", "Minimum",
    "Average Per Min", "Standard Deviation"
  )) {
    this.getSelectionModel.select(0)

    this.onAction = (e: javafx.event.ActionEvent) => update
  }

  val fieldSelect = new ComboBox[String](
    List("Strikes Attempted", "Strikes Landed",
      "Significant Strikes Attempted", "Significant Strikes Landed",
      "Takedowns Attempted", "Takedowns")) {
    this.getSelectionModel.select(1)

    this.onAction = (e: javafx.event.ActionEvent) => update
  }

  val colourSelect = new ComboBox[String](List("Black", "Red", "Gray", "Purple", "Green", "Blue", "Yellow", "Orange")) {
    this.getSelectionModel.select(0)

    this.onAction = (e: ActionEvent) =>
      update
  }

  val label = new Label("Career Total") {font.value = new scalafx.scene.text.Font(size / 10)}
  val valueText = new Label(((fighterInfo.fights.map(_.getField(fieldSelect.value.value, false)).sum * 1000).round.toDouble / 1000).toString)

  def update =
    this.children.clear()

    var value = 0.0

    if fighterInfo.fights.nonEmpty then
      operationSelect.value.value match
        case "Maximum" => value = fighterInfo.fights.map(_.getField(fieldSelect.value.value, false)).max
        case "Minimum" => value = fighterInfo.fights.map(_.getField(fieldSelect.value.value, false)).min
        case "Average Per Min" => value = if fighterInfo.fights.map(_.length).sum != 0 then fighterInfo.fights.map(_.getField(fieldSelect.value.value, false)).sum / fighterInfo.fights.map(_.length).sum else 0
        case "Standard Deviation" =>
          val n = fighterInfo.fights.length
          val avg = fighterInfo.fights.map(_.getField(fieldSelect.value.value, false)).sum / n
          value = math.sqrt(fighterInfo.fights.map(_.getField(fieldSelect.value.value, false)).map(num => (num - avg) * (num - avg)).sum / n)
        case _ => value = fighterInfo.fights.map(_.getField(fieldSelect.value.value, false)).sum

    valueText.text = "  " + ((value * 10000).round.toDouble / 10000).toString

    label.font.value = new scalafx.scene.text.Font(size / 10)
    label.setStyle("-fx-text-fill: " + colourSelect.value.value.toLowerCase)
    valueText.font.value = new scalafx.scene.text.Font(size / 10)
    valueText.setStyle("-fx-text-fill: " + colourSelect.value.value.toLowerCase)
    operationSelect.setStyle("-fx-border-color: " + colourSelect.value.value.toLowerCase + ";\n-fx-font-size:" + size / 10)
    fieldSelect.setStyle("-fx-border-color: " + colourSelect.value.value.toLowerCase + ";\n-fx-font-size:" + size / 10)
    //colourSelect.setStyle("-fx-border-color: " + colourSelect.value.value.toLowerCase + ";\n-fx-font-size:" + size / 10)

    this.setPrefWidth(this.size)
    this.setPrefHeight(this.size)
    this.setMinWidth(this.size)
    this.setMinHeight(this.size)

    add(menuBar, 0, 0)
    add(operationSelect, 0, 1)
    add(fieldSelect, 0, 2)
    add(valueText, 0, 3)
    add(colourSelect, 0, 4)
}

class ScatterPlot (parent: UIElementParent) extends UIElement (parent) {

  override val paddingX: Int = 100
  override val paddingY: Int = 50


  val colourSelect = new ComboBox[String](List("Red", "Black", "Gray", "Purple", "Green", "Blue", "Yellow", "Orange")) {
    this.getSelectionModel.select(0)

    this.onAction = (e: ActionEvent) =>
      update
  }

  val isPerMin = new CheckBox() {
    this.text = "Is per minute"

    this.setPrefWidth(100)

    this.onAction = (e: javafx.event.ActionEvent) =>
      update
  }

  val fieldSelect1 = new ComboBox[String](
    List("Strikes Attempted", "Strikes Landed", "Strike Percentage",
      "Significant Strikes Attempted", "Significant Strikes Landed", "Significant Strike Percentage",
      "Takedowns Attempted", "Takedowns", "Takedown Percentage")) {
    this.getSelectionModel.select(1)

    this.onAction = (e: javafx.event.ActionEvent) => update
  }

  val fieldSelect2 = new ComboBox[String](
    List("Strikes Attempted", "Strikes Landed",
      "Significant Strikes Attempted", "Significant Strikes Landed",
      "Takedowns Attempted", "Takedowns")) {
    this.getSelectionModel.select(1)

    this.onAction = (e: javafx.event.ActionEvent) => update
  }

  def comboBox =
    if isPerMin.selected.value then
      fieldSelect2
    else
      fieldSelect1


  def update =

    val maxVal = fighterInfo.fights.map(point =>
      if (isPerMin.selected.value) then
        point.getField(comboBox.value.apply(), false) / point.length
      else
        point.getField(comboBox.value.apply(), false)).maxOption.getOrElse(100.0)

    val maxDate = fighterInfo.fights.map(_.dateNum).maxOption.getOrElse(2023.0).toInt
    val minDate = fighterInfo.fights.map(_.dateNum).minOption.getOrElse(2015.0).toInt

    val xAxis = new NumberAxis(minDate - 1, maxDate + 1, 1)
    xAxis.setLabel("Year")
    val yAxis = new NumberAxis(-maxVal * 0.2, maxVal * 1.2, maxVal / 10)
    yAxis.setLabel(comboBox.value.apply() + (if isPerMin.selected.value then " per minute" else ""))
    val sc = new ScatterChart[Number, Number](xAxis, yAxis)

    val series =  new XYChart.Series[Number, Number]
    for point <- this.fighterInfo.fights do
      val dataPoint = XYChart.Data[Number, Number](point.dateNum,
        if (isPerMin.selected.value) then point.getField(comboBox.value.apply(), false) / point.length else point.getField(comboBox.value.apply(), false))
      series.getData.add(dataPoint)
    end for

    sc.getData.add(series)

    for point <- scalafx.collections.CollectionIncludes.observableList2ObservableBuffer(series.getData) do
      val fight = this.fighterInfo.fights.find(_.dateNum == point.getXValue)
      val tooltip = new javafx.scene.control.Tooltip(
        s"${comboBox.value.apply()} ${(if isPerMin.selected.value then "per minute" else "")}: ${(point.getYValue.doubleValue() * 1000).round.toDouble / 1000}\n" +
        s"Date: ${fight.getOrElse(new FightData).date}\n" +
        s"${fight.getOrElse(new FightData).description}")

      javafx.scene.control.Tooltip.install(point.getNode, tooltip)
    end for

    for n <- sc.lookupAll(".series" + 0) do
      n.setStyle(s"-fx-background-color: ${colourSelect.value.apply().toLowerCase};")

    sc.setPrefWidth(this.size)
    sc.setPrefHeight(this.size)
    sc.setMinWidth(this.size)
    sc.setMinHeight(this.size)
    sc.legendVisible = false

    this.children.clear()

    this.setPrefWidth(this.size + paddingX)
    this.setPrefHeight(this.size + paddingY)
    this.setMinWidth(this.size + paddingX)
    this.setMinHeight(this.size + paddingY)

    this.add(menuBar, 1, 0)
    this.add(sc, 1, 1, 4, 4)
    this.add(colourSelect, 4, 5)
    this.add(new Label("Select Colour: "), 3, 5)
    this.add(comboBox, 1, 5)
    this.add(isPerMin, 2, 5)

}

class MultiScatterPlot (parent: UIElementParent) extends UIElement (parent) {

  override val paddingX: Int = 250
  override val paddingY: Int = 100

  val isPerMin = new CheckBox() {
    this.text = "Is per minute"

    this.onAction = (e: javafx.event.ActionEvent) =>
      update
  }

  val selectableFields = List("Strikes Attempted", "Strikes Landed",
      "Significant Strikes Attempted", "Significant Strikes Landed",
      "Takedowns Attempted")

  //Contains the FieldSelect instances of this class's selectable fields
  val fieldSelects =
    val b: Buffer[FieldSelect] = Buffer()
    selectableFields.foreach(b += new FieldSelect(_, this))
    b.toList

  val checkList = new GridPane() {
    var x = 0

    while x < fieldSelects.size do
      add(fieldSelects(x).fieldName, 0, x)
      add(fieldSelects(x).isSelected, 1, x)
      add(fieldSelects(x).colourSelect, 2, x)

      x += 1
  }

  def update =

    val selectedFields = fieldSelects.filter(_.isSelected.selected.value)

    val maxVal = selectedFields.map(field => fighterInfo.fights.map(
      (fight: FightData) => if !isPerMin.selected.value then
        fight.getField(field.fieldName.text.value, false) else if fight.length != 0 then
        fight.getField(field.fieldName.text.value, false) / fight.length else 0).maxOption.getOrElse(0.0)).maxOption.getOrElse(100.0)

    val maxDate = fighterInfo.fights.map(_.dateNum).maxOption.getOrElse(2023.0).toInt
    val minDate = fighterInfo.fights.map(_.dateNum).minOption.getOrElse(2015.0).toInt

    val xAxis = new NumberAxis(minDate - 1, maxDate + 1, 1)
    xAxis.setLabel("Year")
    val yAxis = new NumberAxis(-maxVal * 0.2, maxVal * 1.2, maxVal / 10)
    val sc = new ScatterChart[Number, Number](xAxis, yAxis)

    val seriesToAdd: Buffer[XYChart.Series[Number, Number]] = Buffer()

    for field <- selectedFields do
      val series =  new XYChart.Series[Number, Number]
      series.setName(field.fieldName.text.value)
      for point <- this.fighterInfo.fights do
        val pointValue = if !isPerMin.selected.value then point.getField(field.fieldName.text.value, false) else if point.length != 0 then point.getField(field.fieldName.text.value, false) / point.length else 0
        series.getData.add(XYChart.Data[Number, Number](point.dateNum, pointValue))
      end for
      seriesToAdd += series

    seriesToAdd.foreach(sc.getData.add(_))

    //Add tooltip
    var x = 0
    while x < sc.getData.size do
      val series = observableList2ObservableBuffer(sc.getData).apply(x)
      for point <- observableList2ObservableBuffer(series.getData) do
        val fight = this.fighterInfo.fights.find(_.dateNum == point.getXValue)
        val tooltip = new javafx.scene.control.Tooltip(
          s"${selectedFields(x).fieldName.text.value} ${(if isPerMin.selected.value then "per minute" else "")}: ${(point.getYValue.doubleValue() * 1000).round.toDouble / 1000}\n" +
          s"Date: ${fight.getOrElse(new FightData).date}\n" +
          s"${fight.getOrElse(new FightData).description}")

        javafx.scene.control.Tooltip.install(point.getNode, tooltip)
      end for
      x += 1

    var y = 0
    while y < selectedFields.size do
      sc.lookupAll(".series" + y.toString).foreach(_.setStyle(
        s"-fx-background-color: ${selectedFields(y).colourSelect.value.apply().toLowerCase};"))
      y += 1

    sc.legendVisible = false

    sc.setPrefWidth(this.size)
    sc.setPrefHeight(this.size)
    sc.setMinWidth(this.size)
    sc.setMinHeight(this.size)

    this.setPrefWidth(this.size + paddingX)
    this.setPrefHeight(this.size + paddingY)
    this.setMinWidth(this.size + paddingX)
    this.setMinHeight(this.size + paddingY)

    this.children.clear()
    this.add(menuBar, 1, 0)
    this.add(sc, 1, 1, 2, 2)
    this.add(checkList, 3, 1)
    this.add(isPerMin, 3, 2)

}

class BarChart (parent: UIElementParent) extends UIElement (parent) {

  override val paddingX: Int = 250
  override val paddingY: Int = 100

  val isPerMin = new CheckBox() {
    this.text = "Is per minute"

    this.onAction = (e: javafx.event.ActionEvent) =>
      update
  }

  val selectableFields = List("Strikes Attempted", "Strikes Landed",
      "Significant Strikes Attempted", "Significant Strikes Landed",
      "Takedowns Attempted")

  val selectableYears = 1990 to Calendar.getInstance().get(Calendar.YEAR) + 3

  //Contains the FieldSelect instances of this class's selectable fields
  val fieldSelects =
    val b: Buffer[FieldSelect] = Buffer()
    selectableFields.foreach(b += new FieldSelect(_, this))
    b.toList

  val yearSelects =
    val b: Buffer[FieldSelect] = Buffer()
    selectableYears.foreach(year => b += new FieldSelect(year.toString, this))
    b.toList

  def checkList = new GridPane() {
    var x = 0

    while x < fieldSelects.size do
      add(fieldSelects(x).fieldName, 0, x)
      add(fieldSelects(x).isSelected, 1, x)

      x += 1
  }

  def update =

    val selectedFields = fieldSelects.filter(_.isSelected.selected.value)

    val maxVal = selectedFields.map(field => fighterInfo.fights.map(
      (fight: FightData) => fight.getField(field.fieldName.text.value, false)).maxOption.getOrElse(0.0)).maxOption.getOrElse(100.0)

    val years = fighterInfo.fights.map(_.dateNum.toInt).distinct

    val colourList = new GridPane() {
      var x = 0

      while x < years.length do
        val yearSelect = yearSelects.find(_.fieldName.text.value.toInt == years(x)).get
        add(yearSelect.fieldName, 1, x)
        add(yearSelect.colourSelect, 2, x)
        x += 1
    }

    val xAxis = new CategoryAxis()
    val yAxis = new NumberAxis(-maxVal * 0.2, maxVal * 1.2, maxVal / 10)

    var bc = new chart.BarChart(xAxis, yAxis)

    val seriesToAdd: Buffer[XYChart.Series[String, Number]] = Buffer()

    for year <- years do
      val fights = fighterInfo.fights.filter(_.dateNum.toInt == year)
      if fights.nonEmpty then
        val series = new XYChart.Series[String, Number]()
        series.setName(year.toString)

        for field <- selectedFields do
          val fieldValue = fights.map(_.getField(field.fieldName.text.value, false)).sum
          series.getData.add(XYChart.Data[String, Number](field.fieldName.text.value, fieldValue))
        seriesToAdd += series

    seriesToAdd.foreach(bc.getData.add(_))

    for series <- observableList2ObservableBuffer(bc.getData) do
      for point <- series.getData do
        val tooltip = new javafx.scene.control.Tooltip(
          s"${point.getXValue} ${(if isPerMin.selected.value then "per minute" else "")}: ${point.getYValue.toString.dropRight(2)}\n")
        javafx.scene.control.Tooltip.install(point.getNode, tooltip)
    end for

    var x = 0
    while x < years.length do
      bc.lookupAll(".default-color" + x.toString + ".chart-bar").foreach(_.setStyle(
        s"-fx-bar-fill: ${yearSelects.find(_.fieldName.text.value.toInt == years(x)).get.colourSelect.value.apply().toLowerCase};"))
      x += 1

    bc.legendVisible = false

    bc.setPrefWidth(this.size)
    bc.setPrefHeight(this.size)
    bc.setMinWidth(this.size)
    bc.setMinHeight(this.size)

    this.setPrefWidth(this.size + paddingX)
    this.setPrefHeight(this.size + paddingY)
    this.setMinWidth(this.size + paddingX)
    this.setMinHeight(this.size + paddingY)

    this.children.clear()
    this.add(menuBar, 1, 0)
    this.add(bc, 1, 1, 3, 3)
    this.add(checkList, 4, 1)
    this.add(colourList, 4, 2)
    this.add(isPerMin, 4, 3)
}

class PieChart (parent: UIElementParent) extends UIElement (parent) {

  override val paddingX: Int = 250
  override val paddingY: Int = 100

  val isPerMin = new CheckBox() {
    this.text = "Is per minute"

    this.onAction = (e: javafx.event.ActionEvent) =>
      update
  }

  val selectableFields = List("Strikes Attempted", "Strikes Landed",
      "Significant Strikes Attempted", "Significant Strikes Landed",
      "Takedowns Attempted")

  val fieldSelects = selectableFields.map(new FieldSelect(_, this))

  val checkList = new GridPane() {
    var x = 0

    while x < fieldSelects.size do
      add(fieldSelects(x).fieldName, 0, x)
      add(fieldSelects(x).isSelected, 1, x)
      add(fieldSelects(x).colourSelect, 2, x)

      x += 1
  }

  def update =

    val selectedFields = fieldSelects.filter(_.isSelected.selected.value)

    val pc = new chart.PieChart

    for selected <- fieldSelects.filter(_.isSelected.selected.value) do
      val fieldValue = fighterInfo.fights.map(_.getField(selected.fieldName.text.value, false)).sum
      pc.data.value.add(new javafx.scene.chart.PieChart.Data(selected.fieldName.text.value, fieldValue))

    for point <- observableList2ObservableBuffer(pc.getData) do
      val ratio = point.getPieValue / observableList2ObservableBuffer(pc.getData).map(_.getPieValue).sum
      val field = this.fieldSelects.find(_.fieldName.text.value == point.getName).getOrElse(new FieldSelect("", this))
      val tooltip = new javafx.scene.control.Tooltip(
        s"${field.fieldName.text.value} ${(if isPerMin.selected.value then "per minute" else "")}: ${(ratio * 10000).round.toDouble / 100}%\n")

      javafx.scene.control.Tooltip.install(point.getNode, tooltip)
    end for

    var x = 0
    while x < selectedFields.size do
      pc.lookupAll(".default-color" + x.toString + ".chart-pie").foreach(_.setStyle(
        s"-fx-pie-color: ${selectedFields(x).colourSelect.value.apply().toLowerCase};"))
      x += 1

    pc.legendVisible = false

    pc.setPrefWidth(this.size)
    pc.setPrefHeight(this.size)
    pc.setMinWidth(this.size)
    pc.setMinHeight(this.size)

    this.setPrefWidth(this.size + paddingX)
    this.setPrefHeight(this.size + paddingY)
    this.setMinWidth(this.size + paddingX)
    this.setMinHeight(this.size + paddingY)

    this.children.clear()
    this.add(menuBar, 1, 0)
    this.add(pc, 1, 1, 3, 3)
    this.add(new Label("Please select a field") {font.value = new scalafx.scene.text.Font(14)}, 4, 1)
    this.add(checkList, 4, 2)
}