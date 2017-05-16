/* -
 * Mezzo [core]
 */

package mezzo
package tree

import cats.data._

import scala.reflect.api.Universe
import scala.reflect.runtime.{ universe => runtimeUniverse }

object ReflectiveMezzo {
  def apply[U <: Universe](u: U): ReflectiveMezzo[u.type] =
    new ReflectiveMezzo[u.type](u)

  def apply(): ReflectiveMezzo[runtimeUniverse.type] =
    apply(runtimeUniverse)
}

class ReflectiveMezzo[U <: Universe](override val u: U)
    extends Mezzo
{
  override type Uu = U

}

abstract class Mezzo {
  type Uu <: Universe
  val u: Uu

  import u._

  import scala.Predef._


  case class Domain(
    operations: List[Operation]
  )
  case class Operation(name: String, params: List[Param], output: Type)
  case class Param(name: String, tpe: Type)

  def fromADT[F[_]](implicit evT: WeakTypeTag[F[Nothing]]): Domain = {
    val T = evT.tpe.dealias.etaExpand

    val rootSymbol = T.typeSymbol.asClass
    val adts = rootSymbol
      .knownDirectSubclasses
      .filter(_.isClass).map(_.asClass)
      .filter(_.isCaseClass)

    val operations = adts.toList.map { adt =>
      val tpe = adt.toType
      Operation(
        name = adt.name.toString,
        params = tpe.decl(termNames.CONSTRUCTOR).asMethod
          .paramLists.flatten
          .map(p => Param(p.name.toString, p.typeSignature)),
        output = tpe.baseType(rootSymbol).typeArgs.lastOption getOrElse NoType)
    }

    Domain(operations)
  }

  def fromTraitF[F[_[_]]](implicit evF: WeakTypeTag[F[Nothing]]): Domain = {
    val T = evF.tpe.dealias.etaExpand

    val F = T match {
      case PolyType(f :: Nil, res) => f.asType.toType
      case _ => NoType
    }

    val operations = methodOps(T, m => destructF(F, m.returnType))
    Domain(operations)
  }

  def fromObject[T](implicit evT: WeakTypeTag[T]): Domain = {
    val T = evT.tpe.dealias
    val operations = methodOps(T, defleeb)
    Domain(operations)
  }

  trait Fleeb {
    def typeConstructor: Type
    def destruct(tpe: Type): Type

    final def apply(tpe: Type): Option[Type] =
      if (tpe.dealias.typeConstructor <:< typeConstructor) Some(destruct(tpe))
      else None
  }

  object Fleeb {
    def instanceLastArg(tpe: Type): Fleeb = new Fleeb {
      val typeConstructor: Type = tpe.typeConstructor
      def destruct(tpe0: Type): Type =
        tpe0.dealias.typeArgs.lastOption getOrElse NoType
    }
  }

  val KleisliFleeb = Fleeb.instanceLastArg(weakTypeOf[Kleisli[Nothing, _, _]])
  val EitherTFleeb = Fleeb.instanceLastArg(weakTypeOf[EitherT[Nothing, _, _]])
  val OptionTFleeb = Fleeb.instanceLastArg(weakTypeOf[OptionT[Nothing, _]])
  val fleebs: List[Fleeb] = List(KleisliFleeb, EitherTFleeb, OptionTFleeb)

  private[this] def defleeb(
    sym: MethodSymbol
  ): Type = {
    val fa = sym.returnType
    val a = fleebs.map(_ apply fa).collectFirst { case Some(v) => v }
    a getOrElse NoType
  }

  private[this] def methodOps(
    T: Type,
    returnType: MethodSymbol => Type
  ): List[Operation] =
    T.decls
      .filter(_.isTerm).map(_.asTerm)
      .filter(_.isMethod).map(_.asMethod)
      .filter(_.isPublic)
      .filterNot(_.isConstructor)
      .toList
      .map(method => Operation(
        name = method.name.toString.toLowerCase,
        params = method.paramLists.flatten
          .map(p => Param(p.name.toString, p.typeSignature)),
        output = returnType(method)))

  def destructF(F: Type, tpe: Type): Type =
    if (tpe.typeConstructor <:< F)
      tpe.typeArgs.lastOption getOrElse tpe
    else
      tpe


}
