package au.example.app

/**
  * Created by yilmaz on 10/8/16.
  */

case class Point(var x: String, var y: () => Unit)

abstract class PointModel(var name:String) {
  def getBehavior() : Point
  def exec(funcName: String)
}
case class PointFactory(src: String) {
  import reflect.runtime.currentMirror
  import tools.reflect.ToolBox
  val toolbox = currentMirror.mkToolBox()
  import toolbox.u._

  val tree = toolbox.parse(src)
  val compiledCode = toolbox.compile(tree)

  def make() : PointModel = compiledCode().asInstanceOf[PointModel]


}
