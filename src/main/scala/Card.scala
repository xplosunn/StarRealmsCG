import Ops._

case class Card(faction: Faction, cost: Int, cardType: CardType, effects: List[(Effect, Int)])

object RandomCard {
  def apply(): Card = {
    val faction = Faction.values.random
    val cost = (1 to 8).toNEL().random

    val (cardType, effectsCost) = CardTypeGen(cost)

    Card(faction, cost, cardType, EffectGen(faction, effectsCost))
  }

}