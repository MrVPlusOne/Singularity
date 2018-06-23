package singularity.benchmarks;

import org.apache.commons.lang3.tuple.ImmutablePair;
import com.google.common.collect.ImmutableBiMap;

import java.util.ArrayList;
import java.util.Random;


public class GuavaImmutableBiMap {

    public static void main(String[] args) {
        ArrayList<ImmutablePair<Integer, Integer>> pattern = generateInput(600);
        ArrayList<ImmutablePair<Integer, Integer>> randomInput = randomInput(600);

        int testNum = 40000;
        long start;
        double timeUse;

        start = System.nanoTime();
        for(int i = 0; i < testNum; i++) {
            ImmutableBiMap.copyOf(randomInput);
        }
        timeUse = (System.nanoTime() - start)/1e9;
        System.out.println("Random input time use: " + timeUse + "s.");

        start = System.nanoTime();
        for(int i = 0; i < testNum; i++) {
            ImmutableBiMap.copyOf(pattern);
        }
        timeUse = (System.nanoTime() - start)/1e9;
        System.out.println("Pattern time use: " + timeUse + "s.");
    }

    public static ArrayList<ImmutablePair<Integer, Integer>> generateInput(int size){
        int s1 = 475;
        ImmutablePair<Integer, Integer> s2 = new ImmutablePair<>(271,212);
        ArrayList<ImmutablePair<Integer, Integer>> s4 = new ArrayList<>();

        s4.add(s2);

        for (int i = 0; i < size; i ++){
            s1 = ((99*302*486) | 475) + 142 + (s1-1);
            s2 = new ImmutablePair<>(s2.right, s1 << 353);
            s4.add(s2);
        }

        return s4;
    }

    public static ArrayList<ImmutablePair<Integer, Integer>> randomInput(int size){
        Random rand = new Random(1);

        ArrayList<ImmutablePair<Integer, Integer>> list = new ArrayList<>();
        for (int i = 0; i < size; i ++){
            list.add(new ImmutablePair<>(rand.nextInt(), rand.nextInt()));
        }

        return list;
    }
}
