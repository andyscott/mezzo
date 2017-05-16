/* -
 * Mezzo [core]
 */

package mezzo

import scala.Predef._

import mezzo.tree._

object Doof extends App {

  val imz = ReflectiveMezzo()

  sealed trait Foo1[A]
  object Foo1 {
    case class op1(x: Int, y: Long) extends Foo1[Boolean]
    case class op2(value: String) extends Foo1[Option[String]]
    case object op3 extends Foo1[Double]
  }

  val tree1 = imz.fromADT[Foo1]

  trait Foo2[F[_]] {
    type G[A] = F[A]
    def op1(x: Int, y: Long): F[Boolean]
    def op2(value: String): F[Option[String]]
    def op3(): G[Double]
  }

  val tree2 = imz.fromTraitF[Foo2]

  trait Env
  trait FooError

  import cats.data._
  import scala.concurrent.Future
  type Res[A] = Kleisli[EitherT[Future, FooError, ?], Env, A]

  object Foo3 {
    def op1(x: Int, y: Long): Res[Boolean] = ???
    def op2(value: String): Res[Option[String]] = ???
    def op3(): Res[Double] = ???
  }

  val tree3 = imz.fromObject[Foo3.type]

  println(tree1)
  println("")
  println(tree2)
  println("")
  println(tree3)
  println("")

  println(tree1 == tree2)
  println(tree2 == tree3)

  println("*" * 30)

  import imz.u._

  println(weakTypeOf[Either[Nothing, Nothing]].typeConstructor)

}
