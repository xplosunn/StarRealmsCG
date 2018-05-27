import Faction.{Blob, MachineCult, StarEmpire, TradeFederation}
import Ops._

case class GenEffect(probability: Int,
                     costPerEffect: Double,
                     effect: Effect,
                     maxStack: Int => Int)

object EffectGen
  extends CommonEffects
    with TradeFederationEffects
    with MachineCultEffects
    with BlobEffects
    with StarEmpireEffects {

  val allEffects =
    commonEffects ++ Faction.values.toList.flatMap(factionEffects)

  private def factionEffects(faction: Faction) = faction match {
    case TradeFederation => tradeFederationEffects
    case MachineCult => machineCultEffects
    case Blob => blobEffects
    case StarEmpire => starEmpireffects
  }

  def apply(faction: Faction, cost: Int): List[(Effect, Int)] =
    effectGen(cost, commonEffects, factionEffects(faction))

  private def effectGen(cost: Int, genEffects: List[GenEffect], additionalEffects: List[GenEffect] = List.empty, maxEffects: Int = 3): List[(Effect, Int)] = {
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
      effect +: effectGen(remainingCost, remainEffects ++ additionalEffects, maxEffects = maxEffects - 1)
    }).getOrElse(Nil)
  }
}

trait CommonEffects {
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

  private val maxGold: Int => Int = {
    case 2 => 3
    case 3 => 3
    case _ => 4
  }

  val commonEffects = List(
    GenEffect(10, 0.35, Damage, dmgMaxStack),
    GenEffect(10, 0.5, Gold, maxGold),
    GenEffect(2, 2.3, Draw, _ => 2),
    GenEffect(1, 2.3, DestroyBase, _ => 1)
  )
}

trait TradeFederationEffects {
  val tradeFederationEffects = List(
    GenEffect(0, 0.5, LifeGain, _ => 6),
    GenEffect(0, 1, NextAquiredToTop, _ => 1)
  )
}

trait MachineCultEffects {
  val machineCultEffects = List(
    GenEffect(0, 0.7, Scrap, _ => 2)
  )
}

trait BlobEffects {
  val blobEffects = List(
    GenEffect(0, 0.6, ScrapInTradeRow, _ => 2),
    GenEffect(0, 0.9, AcquireForFreeUpTo, _ => 6)
  )
}

trait StarEmpireEffects {
  val starEmpireffects = List(
    GenEffect(0, 0.7, OpponentDiscard, _ => 1),
    GenEffect(0, 2, DiscardAndDraw, _ => 2)
  )
}