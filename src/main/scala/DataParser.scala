import scala.xml
import scala.collection.mutable.Buffer

object DataParser {

  def XMLtoFighterList(xmlFile: xml.Node): Array[(String, String)] =
    val fetchNames = (xmlFile \\ "competitor_ranking" \\ "@name").map(_.text.split(", ").reverse.mkString(" "))
    val fetchIDs = (xmlFile \\ "competitor_ranking" \\ "@id").map(_.text)

    fetchNames.zip(fetchIDs).toArray
    
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

}
