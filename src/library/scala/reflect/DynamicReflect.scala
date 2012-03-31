package scala.reflect
/**
 * A prototype using the reflective compiler
 */
trait DynamicReflect extends Dynamic{
  val target : AnyRef = this
  val target2 = 6
  import scala.reflect.mirror._
  import scala.reflect.runtime.Mirror.ToolBox
  import scala.tools.nsc.reporters._
  import scala.tools.nsc.Settings
  def applyDynamic( method:String )( args:Any* ) = {
    val class_ = target.getClass
    val call = Apply(
      Select(
            TypeApply(
              Select(
                Select(
                  Ident(newFreeVar("foo", symbolForName("scala.reflect.DynamicReflect").asType, this))
                  , newTermName("target")
                ), 
                newTermName("asInstanceOf") )
              , List(TypeTree().setType(classToType(class_)))
            )
        ,newTermName(method)
      )
      ,args.map( x => Literal(Constant(x)) ).toList // FIXME: this of course only works for constant args
    )
    val reporter = new ConsoleReporter(new Settings)
    val toolbox = new ToolBox(reporter,"")
    toolbox.runExpr( call )
  }
}
/**
 * A prototype using Scala reflection
 */
trait DynamicReflect2 extends Dynamic{
  val target : AnyRef = this
  import scala.reflect.mirror._
  def applyDynamic( method:String )( args:Any* ) = {
    val symbol = classToType( target.getClass ).member( newTermName(method) )
    invoke( target, symbol )( args: _* )
  }
}
object TestDynamicReflect extends App{
  object x{
    def test() = 5;
    def testOver(i:Int) = i
    def testOver(s:String) = s
  }

  val d2 = new DynamicReflect2{
    override val target = x
  }
  println( d2.test )
  //println( d2.testOver(1) ) // FAILS due to overloading, java.lang.NoSuchMethodException
  
  val d = new DynamicReflect{ override val target = x }
  println( d.test )
  println( d.testOver(1) )
  println( d.testOver("asdf") )
}