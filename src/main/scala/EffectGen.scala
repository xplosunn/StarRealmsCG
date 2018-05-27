import Ops._

case class GenEffect(probability: Int,
                     costPerEffect: Double,
                     effect: Effect,
                     maxStack: Int => Int)

object EffectGen {
  private val dmgMaxStack: Int => Int = {
    case 1 => 3
    case 2 => 4
    case 3 => 6
    case 4 => 6
    case 5 => 6
    case 6 => 8
    case 7 => 8
    case 8 => 9
  }

  val commonEffects = List(
    GenEffect(10, 0.35, Damage, dmgMaxStack),
    GenEffect(10, 1, Gold, _ => 4),
    GenEffect(2, 2.3, Draw, _ => 2),
    GenEffect(1, 2.3, DestroyBase, _ => 1)
  )

  def apply(faction: Faction, cost: Int): List[(Effect, Int)] =
    effectGen(cost, commonEffects)

  private def effectGen(cost: Int, genEffects: List[GenEffect], maxEffects: Int = 3): List[(Effect, Int)] = {
    val costToSpendOpt = if(cost > 0) Some((1 to cost).toNEL().random) else None

    def effectOpt(costToSpend: Int): Option[(Effect, Int)] = genEffects.filter(_.costPerEffect <= costToSpend).random.map {
      genEffect =>
        (genEffect.effect, Math.min(toIntRoundedUp(costToSpend / genEffect.costPerEffect), genEffect.maxStack(costToSpend)))
    }

    (for {
      costToSpend <- costToSpendOpt
      effect <- effectOpt(costToSpend)
      if maxEffects > 1
    } yield {
      val remainingCost = cost - costToSpend
      val remainEffects = genEffects.filter(_.effect != effect._1)
      effect +: effectGen(remainingCost, remainEffects, maxEffects - 1)
    }).getOrElse(Nil)
  }
}

