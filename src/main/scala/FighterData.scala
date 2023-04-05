class FighterData {

  //You can represent numbers on fights in 2 different way depending on a check box
  //First, raw statistics can be viewed. Second, stats per unit time.

  var name = ""
  var nationality = ""
  var weight = 0.0

  var wins = 0
  var losses = 0
  var draws = 0
  
  var fights: Array[FightData] = Array()

  override def toString: String =
    name + "\n" +
    "Weight: " + weight + "\n" +
    s"Wins $wins Losses $losses Draws $draws" +
    fights.mkString("Array(", ", ", ")")
  
}
