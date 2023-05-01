import scala.collection.mutable.Buffer
import scala.util.{Try, Failure, Success}
import java.io.*
import scala.io.Source
import requests.*


object FileHandler {

  var fighterList: Array[(String, String)] = Array()

  val fightersListLoc = "rankedfighters.xml"

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

  def deleteInDirectory(filePath: String) =
    val file = new File(filePath)

    file.listFiles.foreach(f =>
      if !f.isDirectory then
        f.delete()
    )

  def getFightersAPI() =
    val apiResponse: Response = requests.get("https://api.sportradar.com/mma/trial/v2/en/rankings.xml?api_key=" + APIkey)
    saveFile(fightersListLoc, apiResponse.text())

  def getRankedFighters =
    try
      DataParser.XMLtoFighterList(xml.XML.loadFile(fightersListLoc))
    catch
      case _ =>
        println("Initiating API call")
        getFightersAPI()
        DataParser.XMLtoFighterList(xml.XML.loadFile(fightersListLoc))

  def getFighter(fighterName: String, fighterID: String): FighterData =
    try
      DataParser.XMLtoFighterData(xml.XML.loadFile("FighterData\\Profile-" + fighterName.replace(' ', '-') + ".xml"))
    catch
      case _ =>
        println("Initiating API call.")

        //Get Fighter Profile
        val profileResponse: Response = requests.get(s"https://api.sportradar.com/mma/trial/v2/en/competitors/$fighterID/profile.xml?api_key=$APIkey")
        saveFile("FighterData\\Profile-" + fighterName.replace(' ', '-') + ".xml", profileResponse.text())
        Thread.sleep(1500)

        //Get Fighter Summary
        val summaryResponse: Response = requests.get(s"https://api.sportradar.com/mma/trial/v2/en/competitors/$fighterID/summaries.xml?api_key=$APIkey")
        saveFile("FighterData\\Summary-" + fighterName.replace(' ', '-') + ".xml", summaryResponse.text())

        //Use DataParser to return a FighterData instance
        DataParser.XMLtoFighterData(xml.XML.loadFile("FighterData\\Profile-" + fighterName.replace(' ', '-') + ".xml"))

  def getFighterFromFile(fileName: String) =
    DataParser.XMLtoFighterData(xml.XML.loadFile(fileName))

}
