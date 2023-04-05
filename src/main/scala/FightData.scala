class FightData {

  var description = ""

  //0 win. 1 loss. 2 draw. 3 no contest.
  var result = 0

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
}
