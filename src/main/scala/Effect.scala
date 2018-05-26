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