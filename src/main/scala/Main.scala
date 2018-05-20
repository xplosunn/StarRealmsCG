import Ops._
import cats.data.NonEmptyList

case class Card(faction: Faction, cost: Int, cardType: CardType, effects: NonEmptyList[(Effect, Int)])

object Main {

  def main(args: Array[String]): Unit = {
    println(randomCard)
  }

  def randomCard: Card = {
    val faction = Faction.values.random
    val cost = (1 to 8).toNEL().random

    val (cardType, effectsCost) = CardTypeGen(cost)

    Card(faction, cost, cardType, EffectGen(faction, effectsCost))
  }
}