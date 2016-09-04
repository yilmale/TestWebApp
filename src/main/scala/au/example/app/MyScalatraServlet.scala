package au.example.app

import org.scalatra._




class MyScalatraServlet extends TestWebAppStack  {
  get("/") {
    html.index(new java.util.Date)
  }

  get("/entities") {
    Ok("Got request [" + "]")
  }

}
