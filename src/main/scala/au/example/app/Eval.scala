package au.example.app

/**
  * Created by yilmaz on 10/8/16.
  */
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox
import java.io.File

object Eval {

  def apply[A](string: String): A = {
    val toolbox = currentMirror.mkToolBox()
    val tree = toolbox.parse(string)
    toolbox.eval(tree).asInstanceOf[A]
  }

  def fromFile[A](file: File): A =
    apply(scala.io.Source.fromFile(file).mkString(""))

  def fromFileName[A](file: String): A =
    fromFile(new File(file))

}