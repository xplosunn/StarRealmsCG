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

  property("max stack holds") = forAll(effectGen) { effectList =>
    effectList.forall {
      case (effect, amount) =>
        amount <= findGenEffect(effect).maxStack
    }
  }

  property("effect ammout > 0") = forAll(effectGen) { effectList =>
    effectList.forall {
      case (effect, amount) =>
        amount > 0
    }
  }

  property("acceptable damage") = forAll(effectGenWithCost) { case (effectList, cost) =>
    def belowMaxDmgStack(cost: Int, dmg: Int): Boolean = {
      cost match {
        case 1 => dmg <= 3
        case 2 => dmg <= 4
        case 3 => dmg <= 6
        case 4 => dmg <= 6
        case 5 => dmg <= 6
        case 6 => dmg <= 8
        case 7 => dmg <= 8
        case 8 => dmg <= 9
        case _ => false
      }
    }

    effectList.forall {
      case (_, amount) =>
        belowMaxDmgStack(cost, amount)
    }
  }

  def findGenEffect(effect: Effect) =
    EffectGen.commonEffects.filter(_.effect == effect).head

}
