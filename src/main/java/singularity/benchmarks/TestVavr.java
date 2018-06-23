package singularity.benchmarks;

import patbench.vavr.collection.*;

public class TestVavr {
    public static PriorityQueue<Integer> arrayToPriorityQueue(int[] array) {
        PriorityQueue<Integer> pq = PriorityQueue.empty();
        for (int i = 0; i < array.length; ++i)
            pq = pq.enqueue(i);
        return pq;
    }

    public static Vector<Integer> arrayToVector(int[] array) {
       return Vector.ofAll(array);
    }

    public static TreeSet<Integer> arrayToTreeSet(int[] array) {
        return TreeSet.ofAll(array);
    }

    public static HashSet<Integer> arrayToHashSet(int[] array) {
        return HashSet.ofAll(array);
    }

    public static LinkedHashSet<Integer> arrayToLinkedHashSet(int[] array) {
        return LinkedHashSet.ofAll(array);
    }
}
