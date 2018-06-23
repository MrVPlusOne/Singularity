package singularity.benchmarks;

import patbench.jgrapht.alg.flow.PushRelabelMFImpl;
import patbench.jgrapht.graph.DefaultWeightedEdge;
import patbench.jgrapht.graph.DirectedWeightedMultigraph;


public class BugReproduce {

    public static void main(String[] args) {
        testPushRelabelMFImpl(300);
    }

    public static void testPushRelabelMFImpl(int graphSize){
        int outWeight = 3;
        int inWeight = 4;

        DirectedWeightedMultigraph<Integer, DefaultWeightedEdge> g = new DirectedWeightedMultigraph<>(DefaultWeightedEdge.class);
        for (int i = 0; i<graphSize; i++){
            g.addVertex(i);
        }

        for(int i = 1; i < graphSize; i++) {
            if(i%2 == 0){
                DefaultWeightedEdge eOut = g.addEdge(0, i);
                g.setEdgeWeight(eOut, outWeight);

                DefaultWeightedEdge eInt = g.addEdge(i, 0);
                g.setEdgeWeight(eInt, inWeight);
            }else{
                DefaultWeightedEdge e = g.addEdge(0,i);
                g.setEdgeWeight(e, 0);
            }
        }

        System.out.println("Graph constructed... Size = " + graphSize);

        long startTime = System.nanoTime();
        PushRelabelMFImpl<Integer, DefaultWeightedEdge> algorithm = new PushRelabelMFImpl<>(g);
        algorithm.getMaximumFlow(2,1);

        double time = (System.nanoTime() - startTime)/1e9;
        System.out.println("Finished. Time used: " + time + "s.");
    }
}
