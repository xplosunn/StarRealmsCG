import cats.data.NonEmptyList

sealed trait Faction

object Faction {

  case object Blob extends Faction

  case object StarEmpire extends Faction

  case object TradeFederation extends Faction

  case object MachineCult extends Faction

  val values: NonEmptyList[Faction] =
    NonEmptyList(Blob,
      List(StarEmpire, TradeFederation, MachineCult))
}