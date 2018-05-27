import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}

object EffectGenProps extends Properties("EffectGen") {

  val effectGen = for {
    faction <- Gen.oneOf(Faction.values.toList)
    cost <- Gen.oneOf(1 to 8)
  } yield EffectGen(faction, cost)

  val effectGenWithCost = for {
    faction <- Gen.oneOf(Faction.values.toList)
    cost <- Gen.oneOf(1 to 8)
  } yield (EffectGen(faction, cost), cost)

  property("nonEmpty") = forAll(effectGen) { effectList =>
    effectList.nonEmpty
  }

  property("size <= 3") = forAll(effectGen) { effectList =>
    effectList.length <= 3
  }

  property("effect ammout > 0") = forAll(effectGen) { effectList =>
    effectList.forall {
      case (effect, amount) =>
        amount > 0
    }
  }

  property("max stack holds") = forAll(effectGenWithCost) { case (effectList, cost) =>
    effectList.forall {
      case (effect, amount) =>
        amount <= findGenEffect(effect).maxStack(cost)
    }
  }

  def findGenEffect(effect: Effect): GenEffect =
    EffectGen.commonEffects.filter(_.effect == effect).head

}
