package domain

sealed trait Type {
  def splitAt(target: Type): Option[Seq[Type]]
}
final case class Constant(name: String) extends Type {
  override def splitAt(target: Type): Option[Seq[Type]] =
    if (target == this) Some(Seq.empty)
    else None
}
final case class Arrow(source: Type, target: Type) extends Type {
  override def splitAt(otherTarget: Type): Option[Seq[Type]] =
    if (target == this) Some(Seq.empty)
    else target.splitAt(otherTarget).map(source +: _)
}

final case class Variable(name: String)

case class Context(fixedAssumptions: Map[Variable, Type] = Map.empty) { self =>
  val typeAssumptions: Set[Type] = fixedAssumptions.values.toSet

  def merge(tpe: Type): Context =
    if (typeAssumptions.contains(tpe)) this
    else new Context(fixedAssumptions) {
      override val typeAssumptions: Set[Type] = self.typeAssumptions + tpe
    }
}