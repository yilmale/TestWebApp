package au.example.app

import org.scalatra._


import edu.uci.ics.jung.graph.util.Graphs
import edu.uci.ics.jung.graph.{Graph, ObservableGraph, UndirectedSparseMultigraph}

import reflect.runtime.currentMirror
import tools.reflect.ToolBox


case class Person(name:String)


import scala.reflect.runtime.universe._

class MyScalatraServlet extends TestWebAppStack  {
  get("/") {

    html.index(new java.util.Date)
  }

  post("/parsemodel") {
    val modelSrc = params("model")
    //NetExpr.parseExpression(modelSrc)
    Eval[Unit](modelSrc)

    val bFactory = BehaviorFactory("/Users/yilmaz/external.scala")
    val b = bFactory.make()
    val p = b.getBehavior()



    println(p)

    val ru = scala.reflect.runtime.universe
    val m = ru.runtimeMirror(getClass.getClassLoader)
    val classPerson = ru.typeOf[Person].typeSymbol.asClass
    val cm = m.reflectClass(classPerson)
    val ctor = ru.typeOf[Person].decl(ru.termNames.CONSTRUCTOR).asMethod
    val ctorm = cm.reflectConstructor(ctor)
    val t = ctorm("Levent")

    println(t.asInstanceOf[Person].name)

    val mysrc = txt.test("testtemplate").toString()

    println(mysrc)

    val execsrc = txt.execmodel("au.example.app._","A1",modelSrc).toString()

    println(execsrc)


    val pFactory = PointFactory(execsrc)
    val pb = pFactory.make()
    val myp = pb.getBehavior()

    println(myp)

    myp.y()

    Ok("Parsed data")
  }

  get("/entities") {
    val inpNet : String =
      """
     cogent producer {

       datamodel {
         "This is a string"
       }
       cogModel  {
        cm = X(A,B,C)

        net X(inp: P,Q; outp: Z) = {
          [P->A, Q->B; E->Z]
            data {
              evidence(A,0.5)
              evidence(B:"Test2",0.7)
            }

            hypotheses {
              hypothesis(C:"Test3",0.3)
              hypothesis(D: "Test4",0.9)
              hypothesis(E,0.6)
            }

            constraints {
              C explains B at 0.5
              C contradicts D at 0.7
              [D, E] explains A at 0.6
            }
        }
       }
     }
      """
    //NetExpr.parseExpression(inpNet)

    //val x = html.index.render(new java.util.Date).toString()
    //println(x)



    Ok("Got request [" + "]")
  }

}
