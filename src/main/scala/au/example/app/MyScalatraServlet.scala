package au.example.app

import org.scalatra._


import edu.uci.ics.jung.graph.util.Graphs
import edu.uci.ics.jung.graph.{Graph, ObservableGraph, UndirectedSparseMultigraph}

class MyScalatraServlet extends TestWebAppStack  {
  get("/") {

    html.index(new java.util.Date)
  }

  get("/entities") {
    ParseExpr1.parseExpression("1+2*3")
    Ok("Got request [" + "]")
  }

}
