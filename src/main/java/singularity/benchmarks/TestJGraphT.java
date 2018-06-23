package singularity.benchmarks;

import patbench.jgrapht.Graph;
import patbench.jgrapht.graph.*;
import singularity.EValue;
import singularity.StandardSystem.*;
import scala.Tuple3;
import scala.collection.Iterator;

public class TestJGraphT {
    public static DirectedWeightedMultigraph<Integer, DefaultWeightedEdge> mkDirectedWeightedMultiGraph(GraphValue graphValue){
        DirectedWeightedMultigraph<Integer, DefaultWeightedEdge> g = new DirectedWeightedMultigraph<>(DefaultWeightedEdge.class);
        int n = graphValue.nodeNum();
        for (int i = 0; i<n; i++){
            g.addVertex(i);
        }
        Iterator<Tuple3<Object, Object, EValue>> it = graphValue.edges().iterator();
        while(it.hasNext()){
            Tuple3<Object, Object, EValue> t3 = it.next();
            DefaultWeightedEdge e = g.addEdge((Integer) t3._1(), (Integer) t3._2());
            g.setEdgeWeight(e, ((IntValue)t3._3()).value());
        }
        return g;
    }

    public static SimpleWeightedGraph<Integer, DefaultWeightedEdge> mkSimpleWeightedGraph(GraphValue graphValue){
        SimpleWeightedGraph<Integer, DefaultWeightedEdge> g = new SimpleWeightedGraph<>(DefaultWeightedEdge.class);
        int n = graphValue.nodeNum();
        for (int i = 0; i<n; i++){
            g.addVertex(i);
        }
        Iterator<Tuple3<Object, Object, EValue>> it = graphValue.edges().iterator();
        while(it.hasNext()){
            Tuple3<Object, Object, EValue> t3 = it.next();
            DefaultWeightedEdge e = g.addEdge((Integer) t3._1(), (Integer) t3._2());
            if (e != null)
                g.setEdgeWeight(e, ((IntValue)t3._3()).value());
        }
        return g;
    }

    public static Graph<Integer, DefaultEdge> mkSimpleGraph(GraphValue graphValue) {
        SimpleGraph<Integer, DefaultEdge> g = new SimpleGraph<>(DefaultEdge.class);
        int n = graphValue.nodeNum();
        for (int i = 0; i<n; i++){
            g.addVertex(i);
        }
        Iterator<Tuple3<Object, Object, EValue>> it = graphValue.edges().iterator();
        while(it.hasNext()){
            Tuple3<Object, Object, EValue> t3 = it.next();
            g.addEdge((Integer) t3._1(), (Integer) t3._2());
        }
        return g;
    }
}

