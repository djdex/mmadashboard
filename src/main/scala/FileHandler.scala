import scala.collection.mutable.Buffer
import scala.util.{Try, Failure, Success}
import java.io.*
import scala.io.Source
import requests.*

object FileHandler {

  var fighterList: Array[(String, String)] = Array()

  //REMOVE THIS
  var hasUsed = false

  def APIkey =
    var key = ""
    val reader = new BufferedReader(new FileReader("apikey.txt"))
    key = reader.readLine()
    reader.close()
    key

  def saveFile(filename: String, data: String): Unit =
    val file = new File(filename)
    val writer = new BufferedWriter(new FileWriter(file))
    writer.write(data)
    writer.close()

  def getRankedFighters() =
    val apiResponse: Response = requests.get("https://api.sportradar.com/mma/trial/v2/en/rankings.xml?api_key=" + APIkey)
    saveFile("rankedfighters.xml", apiResponse.text())


  def getFighter(fighterName: String, fighterID: String): FighterData =
    if !hasUsed then
      //Get Fighter Profile
      val profileResponse: Response = requests.get(s"https://api.sportradar.com/mma/trial/v2/en/rankings.xml?api_key=$APIkey")
      saveFile("FighterData\\Profile-" + fighterName.replace(' ', '-') + ".xml", profileResponse.text())
      Thread.sleep(1500)

      //Get Fighter Summary
      val summaryResponse: Response = requests.get(s"https://api.sportradar.com/mma/trial/v2/en/competitors/$fighterID/summaries.xml?api_key=$APIkey")
      saveFile("FighterData\\Summary-" + fighterName.replace(' ', '-') + ".xml", summaryResponse.text())
      hasUsed = true
      //Use DataParser to return a FighterData instance
      DataParser.XMLtoFighterData(xml.XML.loadFile("FighterData\\Profile-" + fighterName.replace(' ', '-') + ".xml"))

    else
      DataParser.XMLtoFighterData(xml.XML.loadFile("MMA_v2_Competitor_Profile_Example.xml"))
}
