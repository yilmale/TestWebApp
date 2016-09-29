package au.example.app


import edu.uci.ics.jung.graph._
import edu.uci.ics.jung.graph.util.Graphs


import scala.util.parsing.combinator._

/**
  * Created by Levent Yilmaz on 9/21/2016.
  */


class CoherenceModel(mName:String) {
  var g: Graph[String,Int] = null;
  var og: ObservableGraph[String,Int] =null;
  var ig = Graphs.synchronizedUndirectedGraph[String,Int](new  UndirectedSparseMultigraph[String,Int]())
  var activations:Map[String,Double]  = Map()
  var edgeWeights:Map[Int,Double] = Map()
  og = new ObservableGraph[String,Int](ig)
  g = og;

  def addNode(nodeName: String, defaultActivation: Double): Unit = {
    g.addVertex(nodeName)
    activations+=(nodeName -> defaultActivation)
  }

  def addEdge(source: String, target: String, weight: Double): Unit = {
    val edgeCount = g.getEdgeCount
    g.addEdge(edgeCount,source, target)
    edgeWeights+= (edgeCount -> weight)
  }

  def is (body : => Unit): CoherenceModel = {
    body
    this
  }

  def evidence(eName: String, defaultActivation:Double) = {
    addNode(eName,defaultActivation)
  }

  def facilitate(src: String, tgt: String, strength: Double)  = {
    addEdge(src,tgt,strength)
  }

  def graphIterator() {

    var V = g.getVertices
    var itr = V.iterator()
    System.out.println("Vertex List: ")
    while (itr.hasNext()) {
      var myN = itr.next()
      System.out.print("Node name: "+myN+ " ")
      System.out.println("Activation: "+ activations(myN))

      var incIter = g.getIncidentEdges(myN).iterator()
      while (incIter.hasNext()) {
        var myEdge = incIter.next()
        System.out.println("Incident edge number: "+ myEdge+" Weight is " + edgeWeights(myEdge) +" Source is "+g.getEndpoints(myEdge).getFirst+" Target is "+ g.getEndpoints(myEdge).getSecond)
        System.out.println("The opposite vertex of"+myN+" in edge"+ myEdge+" is  "+ g.getOpposite(myN,myEdge))
      }
    }
  }
}


object CoherenceModel  {
  var cm : CoherenceModel = _
  def apply(x:String): CoherenceModel = {
    cm = new CoherenceModel(x)
    cm
  }

  def evidence(eName: String, defaultActivation:Double) = {
    cm.addNode(eName,defaultActivation)
  }

  def facilitate(src: String, tgt: String, strength: Double)  = {
    cm.addEdge(src,tgt,strength)
  }

}

/*
abstract class NetModel
case class Network(name: String) extends NetModel
class CoherenceNet extends JavaTokenParsers {
  def netexpr: Parser[NetModel] = "network"~ident~"="~"{"~evidenceList~"}" ^^ {case ident => Network(ident.toString)}
  //def evidenceList: Parser[NetModel] = "evidences"

}
*/
