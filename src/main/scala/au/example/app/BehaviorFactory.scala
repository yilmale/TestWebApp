package au.example.app

/**
  * Created by yilmaz on 10/8/16.
  */

abstract class Mechanism

case class StepBehavior() extends Mechanism

abstract class Behavior {
  def getBehavior() : Mechanism
}

case class BehaviorFactory(implFileName: String) {
  import reflect.runtime.currentMirror
  import tools.reflect.ToolBox
  val toolbox = currentMirror.mkToolBox()
  import toolbox.u._
  import io.Source

  val fileContents = Source.fromFile(implFileName).getLines.mkString("\n")
  val tree = toolbox.parse("import au.example.app._; " + fileContents)
  val compiledCode = toolbox.compile(tree)

  def make() : Behavior = compiledCode().asInstanceOf[Behavior]
}
