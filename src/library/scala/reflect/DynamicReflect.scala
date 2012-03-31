package scala.reflect
/**
 * A prototype using the reflective compiler
 */
trait DynamicReflect extends Dynamic{
  val target : AnyRef = this
  import scala.reflect.mirror._
  import scala.reflect.runtime.Mirror.ToolBox
  import scala.tools.nsc.reporters._
  import scala.tools.nsc.Settings
  def applyDynamic( method:String )( args:Any* ) = {
    val class_ = target.getClass
    val call = Apply(
      Select(
          Apply(
            TypeApply(
              Select(
                Select(
                  This(newTypeName("DynamicReflect"))
                  , newTermName("target")
                ), 
                newTermName("asInstanceOf") )
              , List(TypeTree().setType(classToType(class_)))
            )
          , List() ),
          newTermName(method)
      )
      ,args.map( x => Literal(Constant(x)) ).toList
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
    def test = 5;
    def testOver(i:Int) = i
    def testOver(s:String) = s
  }

  val d2 = new DynamicReflect2{
    override val target = x
  }
  println( d2.test ) // works, prints 5
  // println( d2.testOver(1) ) // FAILS due to overloading, java.lang.NoSuchMethodException
  
  val d = new DynamicReflect{ override val target = x }
  println( d.test ) // FAILS: DynamicReflect is not an enclosing class
}