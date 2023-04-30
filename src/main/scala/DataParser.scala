import javafx.scene.chart.ScatterChart

import scala.xml
import scala.collection.mutable.Buffer

object DataParser {

  //Uses the asdads.xml file to generate a list of fighters, with their corresponding IDs
  def XMLtoFighterList(xmlFile: xml.Node) =
    val fetchClasses = (xmlFile \ "ranking")
    val fighters = fetchClasses.map(node => ((node \ "@name").text,
      ((node \\ "competitor_ranking" \\ "@name").map((n: xml.Node) => (n.text.split(", ").reverse.mkString(" ")))).zip((node \\ "competitor_ranking" \\ "@id").map(_.text))
    ))

    fighters

  //Takes a given XML profile file (summary file is needed too) and turns it into a usable FighterData instance
  def XMLtoFighterData(xmlFile: xml.Node): FighterData =
    val fighterInfo = new FighterData

    val fighterID = (xmlFile \\ "@id").text
    val fighterName = (xmlFile \\ "@name").text.split(", ").reverse.mkString(" ")

    //Get basic info from the fighter profile
    fighterInfo.name = fighterName
    fighterInfo.nationality = (xmlFile \\ "@birth_country").text
    fighterInfo.weight = (xmlFile \\ "@weight").text.toDouble
    fighterInfo.wins = (xmlFile \\ "@wins").text.toInt
    fighterInfo.losses = (xmlFile \\ "@losses").text.toInt
    fighterInfo.draws = (xmlFile \\ "@draws").text.toInt


    //Get info about the individual fights from the fighter summary
    val summary = xml.XML.loadFile("FighterData\\Summary-" + fighterName.replace(' ', '-') + ".xml")

    val fights: Buffer[FightData] = Buffer()
    val fightNodes = (summary \\ "summary").filter(n => (n \\ "statistics").nonEmpty)

    for node <- fightNodes do
      val fight = new FightData

      val thisStats = (node \ "statistics" \\ "totals" \\ "competitor").filter(n => (n \\ "@id").text == fighterID)
      val oppStats = (node \ "statistics" \\ "totals" \\ "competitor").filter(n => (n \\ "@id").text != fighterID)
      val oppName = (oppStats \\ "@name").text

      fight.date =
        val text = (node \ "sport_event" \ "@start_time").text
        text.substring(8, 10) + "." + text.substring(5, 7) + "." + text.take(4)

      fight.dateNum =
        fight.date.takeRight(4).toDouble + fight.date.substring(3, 5).toDouble / 12 + fight.date.take(2).toDouble / 360

      fight.length =
        val fullRounds = (node \ "sport_event_status" \\ "@final_round").text.toInt - 1
        val lastRoundText = (node \ "sport_event_status" \\ "@final_round_length").text
        val lastRoundTime = lastRoundText.take(1).toDouble + (lastRoundText.takeRight(2).toDouble / 60)
        5.0 * fullRounds + lastRoundTime

      fight.knockdowns = (thisStats \\ "@knockdowns").text.toIntOption.getOrElse(0)

      fight.totalStrikes = (thisStats \\ "@total_strikes").text.toIntOption.getOrElse(0)
      fight.totalStrikeAttempts = (thisStats \\ "@total_strikes_attempted").text.toIntOption.getOrElse(0)

      fight.sigStrikes = (thisStats \\ "@significant_strikes").text.toIntOption.getOrElse(0)
      fight.sigStrikeAttempts = (thisStats \\ "@significant_strikes_attempted").text.toIntOption.getOrElse(0)

      fight.takedowns = (thisStats \\ "@takedowns").text.toIntOption.getOrElse(0)
      fight.takedownAttempts = (thisStats \\ "@takedowns_attempted").text.toIntOption.getOrElse(0)

      fight.oppKnockdowns = (oppStats \\ "@knockdowns").text.toIntOption.getOrElse(0)

      fight.oppTotalStrikes = (oppStats \\ "@total_strikes").text.toIntOption.getOrElse(0)
      fight.oppTotalStrikeAttempts = (oppStats \\ "@total_strikes_attempted").text.toIntOption.getOrElse(0)

      fight.oppSigStrikes = (oppStats \\ "@significant_strikes").text.toIntOption.getOrElse(0)
      fight.oppSigStrikeAttempts = (oppStats \\ "@significant_strikes_attempted").text.toIntOption.getOrElse(0)

      fight.oppTakedowns = (oppStats \\ "@takedowns").text.toIntOption.getOrElse(0)
      fight.oppTakedownAttempts = (oppStats \\ "@takedowns_attempted").text.toIntOption.getOrElse(0)

      fight.description = s"vs. ${oppName}\n${(node \\ "sport_event" \\ "competition" \\ "@name").text}"

      fights += fight
    end for

    fighterInfo.fights = fights.toArray

    fighterInfo

  def elementsToXML(elements: Seq[UIElementParent]) =
    var xmlText = "<dashboard_save>\n"
    val uiElements = elements.map(_.getChild)


    for elem <- uiElements do


      elem match
        case sc: ScatterPlot =>
          xmlText += "<element "
          xmlText += "type=\"scatterplot\" "
          xmlText += s"size=\"${sc.size.toString}\" "
          xmlText += s"selected_field=\"${sc.comboBox.value.value}\" "
          xmlText += s"colour=\"${sc.colourSelect.value.value}\" "
          xmlText += s"is_per_min=\"${sc.isPerMin.selected.value}\"/>\n"

        case mc: MultiScatterPlot =>
          xmlText += "<element "
          xmlText += "type=\"multiscatterplot\" "
          xmlText += s"size=\"${mc.size.toString}\" "
          xmlText += s"is_per_min=\"${mc.isPerMin.selected.value}\">\n"

          mc.fieldSelects.filter(_.isSelected.selected.value).foreach( field =>
            xmlText += s"<field name=\"${field.fieldName.text.value}\" "
            xmlText += s"colour=\"${field.colourSelect.value.value}\"/>\n"
          )
          xmlText += "</element>\n"

        case bc: BarChart =>
          xmlText += "<element "
          xmlText += "type=\"barchart\" "
          xmlText += s"size=\"${bc.size.toString}\" "
          xmlText += s"is_per_min=\"${bc.isPerMin.selected.value}\">\n"

          val selectedFields = bc.fieldSelects.filter(_.isSelected.selected.value)

          for field <- selectedFields do
            xmlText += s"<field name=\"${field.fieldName.text.value}\"/>"

          val years = bc.yearSelects
          for year <- years do
            xmlText += s"<year name=\"${year.fieldName.text.value}\" colour=\"${year.colourSelect.value.value}\"/>"

          xmlText += "</element>\n"

        case pc: PieChart =>
          xmlText += "<element "
          xmlText += "type=\"piechart\" "
          xmlText += s"size=\"${pc.size.toString}\" "
          xmlText += s"is_per_min=\"${pc.isPerMin.selected.value}\">\n"

          pc.fieldSelects.foreach( field =>
            xmlText += s"<field name=\"${field.fieldName.text.value}\" "
            xmlText += s"colour=\"${field.colourSelect.value.value}\" "
            xmlText += s"is_selected=\"${field.isSelected.selected.value}\"/>\n"
          )
          xmlText += "</element>\n"

        case sc: StatCard =>
          xmlText += "<element "
          xmlText += "type=\"statcard\" "
          xmlText += s"size=\"${sc.size.toString}\" "
          xmlText += s"selected_operation=\"${sc.operationSelect.value.value}\" "
          xmlText += s"selected_field=\"${sc.fieldSelect.value.value}\" "
          xmlText += s"colour=\"${sc.colourSelect.value.value}\" "
          xmlText += "/>\n"
        case _ =>

    xmlText += "</dashboard_save>"
    xmlText

  def xmlToElements(xmlFile: xml.Node, deleteDuplicate: ((UIElementParent, Boolean) => Unit)): Seq[UIElementParent] =
    val elements: Buffer[UIElementParent] = Buffer()

    val xmlElements = (xmlFile \\ "element")

    var x = 0
    for elem <- xmlElements do
      (elem \ "@type").text match
        case "scatterplot" =>
          val parent = new UIElementParent(deleteDuplicate)
          val sc = new ScatterPlot(parent)
          parent.elementSelect.getSelectionModel.select("Scatter Plot")
          parent.setChild(sc)
          sc.size = (elem \\ "@size").text.toInt
          sc.isPerMin.selected.value = (elem \\ "@is_per_min").text == "true"
          sc.comboBox.getSelectionModel.select(sc.comboBox.items.value.indexOf((elem \\ "@selected_field").text))
          sc.colourSelect.getSelectionModel.select(sc.colourSelect.items.value.indexOf((elem \\ "@colour").text))
          elements += parent

        case "multiscatterplot" =>
          val parent = new UIElementParent(deleteDuplicate)
          val mc = new MultiScatterPlot(parent)
          parent.elementSelect.getSelectionModel.select("MultiSeries Plot")
          parent.setChild(mc)
          mc.size = (elem \\ "@size").text.toInt
          mc.isPerMin.selected.value = (elem \\ "@is_per_min").text == "true"

          for field <- (elem \\ "field") do
            mc.fieldSelects.filter(_.fieldName.text.value == (field \\ "@name").text).foreach(f =>
              f.isSelected.selected.value = true;
              f.colourSelect.getSelectionModel.select((field \\ "@colour").text))
          elements += parent

        case "barchart" =>
          val parent = new UIElementParent(deleteDuplicate)
          val bc = new BarChart(parent)
          parent.elementSelect.getSelectionModel.select("Bar Chart")
          parent.setChild(bc)
          bc.size = (elem \ "@size").text.toInt
          bc.isPerMin.selected.value = (elem \ "@is_per_min").text == "true"

          for field <- (elem \\ "field") do
            val matchingField = bc.fieldSelects.find(_.fieldName.text.value == (field \\ "@name").text).getOrElse(new FieldSelect("", bc))
            matchingField.isSelected.selected.value = true

          for year <- (elem \ "year") do
            val matchingField = bc.yearSelects.find(_.fieldName.text.value == (year \\ "@name").text).getOrElse(new FieldSelect("", bc))
            matchingField.colourSelect.getSelectionModel.select((if (year \\ "@colour").text != "" then (year \\ "@colour").text else "Red"))
          elements += parent

        case "piechart" =>
          val parent = new UIElementParent(deleteDuplicate)
          val pc = new PieChart(parent)
          parent.elementSelect.getSelectionModel.select("Pie Chart")
          parent.setChild(pc)
          pc.size = (elem \ "@size").text.toInt
          pc.isPerMin.selected.value = (elem \ "@is_per_min").text == "true"

          for field <- (elem \ "field") do
            pc.fieldSelects.filter(_.fieldName.text.value == (field \\ "@name").text).foreach(f =>
              f.isSelected.selected.value = if (field \\ "@is_selected").text == "true" then true else false;
              f.colourSelect.getSelectionModel.select((field \\ "@colour").text))
          elements += parent
        case "statcard" =>
          val parent = new UIElementParent(deleteDuplicate)
          val sc = new StatCard(parent)
          parent.elementSelect.getSelectionModel.select("Scatter Plot")
          parent.setChild(sc)
          sc.size = (elem \\ "@size").text.toInt
          sc.operationSelect.getSelectionModel.select(sc.operationSelect.items.value.indexOf((elem \\ "@selected_operation").text))
          sc.fieldSelect.getSelectionModel.select(sc.fieldSelect.items.value.indexOf((elem \\ "@selected_field").text))
          sc.colourSelect.getSelectionModel.select(sc.colourSelect.items.value.indexOf((elem \\ "@colour").text))
          elements += parent
        case _ =>

    elements.foreach(_.getChild.update)
    elements.toSeq

  def duplicateElement(element: UIElementParent): UIElementParent =

    val deleteDuplicate = element.deleteDuplicate

    element.getChild match
      case original: ScatterPlot =>
        val parent = new UIElementParent(deleteDuplicate)
        val newElem = new ScatterPlot(parent)
        parent.setChild(newElem)
        parent.elementSelect.getSelectionModel.select("Scatter Plot")
        newElem.size = original.size
        newElem.isPerMin.selected.value = original.isPerMin.selected.value
        newElem.comboBox.getSelectionModel.select(original.comboBox.value.value)
        newElem.colourSelect.getSelectionModel.select(original.colourSelect.value.value)
        newElem.update
        parent

      case original: MultiScatterPlot =>
        val parent = new UIElementParent(deleteDuplicate)
        val newElem = new MultiScatterPlot(parent)
        parent.setChild(newElem)
        parent.elementSelect.getSelectionModel.select("MultiSeries Plot")
        newElem.size = original.size
        newElem.isPerMin.selected.value = original.isPerMin.selected.value

        for field <- original.fieldSelects do
          val matchingField = newElem.fieldSelects.find(_.fieldName.text.value == field.fieldName.text.value).get;
          matchingField.isSelected.selected.value = field.isSelected.selected.value
          matchingField.colourSelect.getSelectionModel.select(field.colourSelect.value.value)

        newElem.update
        parent

      case original: BarChart =>
        val parent = new UIElementParent(deleteDuplicate)
        val newElem = new BarChart(parent)
        parent.setChild(newElem)
        parent.elementSelect.getSelectionModel.select("Bar Chart")
        newElem.size = original.size
        newElem.isPerMin.selected.value = original.isPerMin.selected.value

        for field <- original.fieldSelects do
          val matchingField = newElem.fieldSelects.find(_.fieldName.text.value == field.fieldName.text.value).get;
          matchingField.isSelected.selected.value = field.isSelected.selected.value

        val years = original.yearSelects
        var x = 0
        while x < original.yearSelects.size do
          newElem.yearSelects(x).colourSelect.getSelectionModel.select(original.yearSelects(x).colourSelect.value.value)
          x += 1

        newElem.update
        parent

      case original: PieChart =>
        val parent = new UIElementParent(deleteDuplicate)
        val newElem = new PieChart(parent)
        parent.setChild(newElem)
        parent.elementSelect.getSelectionModel.select("Pie Chart")
        newElem.size = original.size
        newElem.isPerMin.selected.value = original.isPerMin.selected.value

        for field <- original.fieldSelects do
          val matchingField = newElem.fieldSelects.find(_.fieldName.text.value == field.fieldName.text.value).get;
          matchingField.isSelected.selected.value = field.isSelected.selected.value
          matchingField.colourSelect.getSelectionModel.select(field.colourSelect.value.value)

        newElem.update
        parent

      case original: StatCard =>
        val parent = new UIElementParent(deleteDuplicate)
        val newElem = new StatCard(parent)
        parent.setChild(newElem)
        parent.elementSelect.getSelectionModel.select("Stat Card")
        newElem.size = original.size
        newElem.operationSelect.getSelectionModel.select(original.operationSelect.value.value)
        newElem.fieldSelect.getSelectionModel.select(original.fieldSelect.value.value)
        newElem.colourSelect.getSelectionModel.select(original.colourSelect.value.value)
        newElem.update
        parent
      case _ =>
        new UIElementParent(deleteDuplicate)
}
