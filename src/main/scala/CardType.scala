import CardType.{Base, Ship}

import scala.util.Random

sealed trait CardType

object CardType {
  case object Ship extends CardType
  case class Base(life: Int, advanced: Boolean) extends CardType
}

object CardTypeGen {
  def apply(cost: Int): (CardType, Int) = {
    if(cost > 1 && Random.nextBoolean()) {
      val advanced = Random.nextBoolean()
      val life = if(advanced) Math.min(cost- 1, 6) else cost

      (Base(life, advanced), if(advanced) cost - 3 else cost - 2)
    } else {
      (Ship, cost)
    }
  }
}
