import scala.xml
import scala.collection.mutable.Buffer

object DataParser {

  def XMLtoFighterList(xmlFile: xml.Node) =
    val fetchClasses = (xmlFile \ "ranking")
    val fighters = fetchClasses.map(node => ((node \ "@name").text,
      ((node \\ "competitor_ranking" \\ "@name").map((n: xml.Node) => (n.text.split(", ").reverse.mkString(" ")))).zip((node \\ "competitor_ranking" \\ "@id").map(_.text))
    ))

    fighters

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
        println(text)
        text.substring(8, 10) + "." + text.substring(5, 7) + "." + text.take(4)

      fight.dateNum =
        fight.date.takeRight(4).toDouble + fight.date.substring(3, 5).toDouble / 12 + fight.date.take(2).toDouble / 360

      fight.length =
        val fullRounds = (node \ "sport_event_status" \\ "@final_round").text.toInt - 1
        println(fullRounds)
        val lastRoundText = (node \ "sport_event_status" \\ "@final_round_length").text
        val lastRoundTime = lastRoundText.take(1).toDouble + (lastRoundText.takeRight(2).toDouble / 60)
        println(lastRoundText.takeRight(2))
        5.0 * fullRounds + lastRoundTime

      fight.knockdowns = (thisStats \\ "@knockdowns").text.toIntOption.getOrElse(-1)

      fight.totalStrikes = (thisStats \\ "@total_strikes").text.toIntOption.getOrElse(-1)
      fight.totalStrikeAttempts = (thisStats \\ "@total_strikes_attempted").text.toIntOption.getOrElse(-1)

      fight.sigStrikes = (thisStats \\ "@significant_strikes").text.toIntOption.getOrElse(-1)
      fight.sigStrikeAttempts = (thisStats \\ "@significant_strikes_attempted").text.toIntOption.getOrElse(-1)

      fight.takedowns = (thisStats \\ "@takedowns").text.toIntOption.getOrElse(-1)
      fight.takedownAttempts = (thisStats \\ "@takedowns_attempted").text.toIntOption.getOrElse(-1)

      fight.oppKnockdowns = (oppStats \\ "@knockdowns").text.toIntOption.getOrElse(-1)

      fight.oppTotalStrikes = (oppStats \\ "@total_strikes").text.toIntOption.getOrElse(-1)
      fight.oppTotalStrikeAttempts = (oppStats \\ "@total_strikes_attempted").text.toIntOption.getOrElse(-1)

      fight.oppSigStrikes = (oppStats \\ "@significant_strikes").text.toIntOption.getOrElse(-1)
      fight.oppSigStrikeAttempts = (oppStats \\ "@significant_strikes_attempted").text.toIntOption.getOrElse(-1)

      fight.oppTakedowns = (oppStats \\ "@takedowns").text.toIntOption.getOrElse(-1)
      fight.oppTakedownAttempts = (oppStats \\ "@takedowns_attempted").text.toIntOption.getOrElse(-1)

      fight.description = s"vs. ${oppName}\n${(node \\ "sport_event" \\ "competition" \\ "@name").text}"

      fights += fight
    end for

    fighterInfo.fights = fights.toArray

    fighterInfo

  def elementsToXML(elements: Seq[scalafx.scene.Node], fileName: String) =
    val uiElements = elements.filter(_.isInstanceOf[UIElement]).map(_.asInstanceOf[UIElement])

}
