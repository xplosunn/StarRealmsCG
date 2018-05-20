import Ops._
import cats.data.NonEmptyList

sealed trait Effect

case object Damage extends Effect

case object Gold extends Effect

case object Draw extends Effect

case object DestroyBase extends Effect

// Trade Federation
case object LifeGain extends Effect

case object NextAquiredToTop extends Effect

//  Machine Cult
case object Scrap extends Effect

// Blob
case object ScrapInTradeRow extends Effect

case object AcquireForFreeUpTo extends Effect

// Star Empire
case object OpponentDiscard extends Effect

case object DiscardAndDraw extends Effect

case class GenEffect(probability: Int,
                     costPerEffect: Double,
                     effect: Effect,
                     maxStack: Int)

object EffectGen {
  val commonEffects = NonEmptyList(
    GenEffect(10, 0.35, Damage, 3),
    List(GenEffect(10, 1, Gold, 4),
         GenEffect(2, 2.3, Draw, 2),
         GenEffect(1, 2.3, DestroyBase, 1))
  )

  def apply(faction: Faction, cost: Int): NonEmptyList[(Effect, Int)] =
    genHelper(cost, commonEffects)

  private def genHelper(cost: Int,
                        effects: NonEmptyList[GenEffect],
                        maxEffects: Int = 2): NonEmptyList[(Effect, Int)] = {
    val genEffect: GenEffect =
      effects /*.filter(_.costPerEffect < cost)*/.randomWeighed(_.probability)
    val costToSpend =
      if (maxEffects > 0)
        (1 to Math.min(genEffect.maxStack, cost)).toNEL().random
      else
        cost

    val tail = {
      val remainingEffects = effects.filter(_ != genEffect)
      val remainingCost = cost - costToSpend

      NonEmptyList
        .fromList(remainingEffects)
        .filter(_ => remainingCost > 0 && maxEffects > 1)
        .map(genHelper(remainingCost, _, maxEffects - 1).toList)
        .getOrElse(Nil)
    }

    NonEmptyList(
      (genEffect.effect, toIntRoundedUp(costToSpend / genEffect.costPerEffect)),
      tail)
  }
}
