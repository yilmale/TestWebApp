package au.example.app

import org.scalatra._


import edu.uci.ics.jung.graph.util.Graphs
import edu.uci.ics.jung.graph.{Graph, ObservableGraph, UndirectedSparseMultigraph}



class MyScalatraServlet extends TestWebAppStack  {
  get("/") {
    val x = txt.test.render("testtemplate").toString()
    println(x)
    html.index(new java.util.Date)

  }

  get("/entities") {
    ParseExpr1.parseExpression("1+4*3")
    Ok("Got request [" + "]")
  }

}
