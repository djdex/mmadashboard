class FightData {

  var description = ""
  
  var date = ""

  var dateNum = 0.0

  var length = 0.0

  var knockdowns = 0

  var totalStrikes = 0
  var totalStrikeAttempts = 0

  var sigStrikes = 0
  var sigStrikeAttempts = 0

  var takedowns = 0
  var takedownAttempts = 0

  var oppKnockdowns = 0

  var oppTotalStrikes = 0
  var oppTotalStrikeAttempts = 0
  
  var oppSigStrikes = 0
  var oppSigStrikeAttempts = 0

  var oppTakedowns = 0
  var oppTakedownAttempts = 0

  override def toString: String =
    description + "\n" + s"$totalStrikes $sigStrikes $takedownAttempts"

  //This method returns a given value from a string.
  //Called by UIElements to get data corresponding to the user's selection.
  def getField(field: String, isOpponent: Boolean): Double =
    field match
      case "Strikes Attempted" => if isOpponent then oppTotalStrikeAttempts else totalStrikeAttempts
      case "Strikes Landed" => if isOpponent then oppTotalStrikes else totalStrikes
      case "Strike Percentage" => if isOpponent && oppTotalStrikeAttempts != 0 then oppTotalStrikes.toDouble / oppTotalStrikeAttempts
        else if totalStrikeAttempts != 0 then totalStrikes.toDouble / totalStrikeAttempts else 0
      case "Significant Strikes Attempted" => if isOpponent then oppSigStrikeAttempts else sigStrikeAttempts
      case "Significant Strikes Landed" => if isOpponent then oppSigStrikes else sigStrikes
      case "Significant Strike Percentage" => if isOpponent && oppSigStrikeAttempts != 0 then oppSigStrikes.toDouble / oppSigStrikeAttempts
        else if sigStrikeAttempts != 0 then sigStrikes.toDouble / sigStrikeAttempts else 0
      case "Takedowns Attempted" => if isOpponent then oppTakedownAttempts else takedownAttempts
      case "Takedowns" => if isOpponent then oppTakedowns else takedowns
      case "Takedown Percentage" => if isOpponent && oppTakedownAttempts != 0 then oppTakedowns.toDouble / oppTakedownAttempts
        else if takedownAttempts != 0 then takedowns.toDouble / takedownAttempts else 0
      case _ => 0
}
