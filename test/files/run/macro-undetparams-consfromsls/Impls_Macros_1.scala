import scala.reflect.runtime.universe._
import scala.reflect.makro.Context

object Macros {
  def cons_impl[A: c.TypeTag](c: Context)(x: c.Expr[A], xs: c.Expr[List[A]]): c.Expr[List[A]] = c.reify {
    println("A = " + c.literal(implicitly[c.TypeTag[A]].toString).splice)
    x.splice :: xs.splice
  }

  def nil_impl[B: c.TypeTag](c: Context): c.Expr[List[B]] = c.reify {
    println("B = " + c.literal(implicitly[c.TypeTag[B]].toString).splice)
    Nil
  }

  def cons[A](x: A, xs: List[A]): List[A] = macro cons_impl[A]

  def nil[B]: List[B] = macro nil_impl[B]
}