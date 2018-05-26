import Ops._

case class GenEffect(probability: Int,
                     costPerEffect: Double,
                     effect: Effect,
                     maxStack: Int)

object EffectGen {
  val commonEffects = List(
    GenEffect(10, 0.35, Damage, 3),
    GenEffect(10, 1, Gold, 4),
    GenEffect(2, 2.3, Draw, 2),
    GenEffect(1, 2.3, DestroyBase, 1)
  )

  def apply(faction: Faction, cost: Int): List[(Effect, Int)] =
    effectGen(cost, commonEffects)

  private def effectGen(cost: Int, genEffects: List[GenEffect]): List[(Effect, Int)] = {
    val costToSpendOpt = if(cost > 0) Some((1 to cost).toNEL().random) else None

    def effectOpt(costToSpend: Int) = genEffects.filter(_.costPerEffect > costToSpend).random.map {
      genEffect =>
        (genEffect.effect, toIntRoundedUp(Math.min(costToSpend, genEffect.maxStack) / genEffect.costPerEffect))
    }

    (for {
      costToSpend <- costToSpendOpt
      effect <- effectOpt(costToSpend)
    } yield {
      val remainingCost = cost - costToSpend
      val remainEffects = genEffects.filter(_.effect != effect._1)
      effect +: effectGen(remainingCost, remainEffects)
    }).getOrElse(Nil)
  }
}

