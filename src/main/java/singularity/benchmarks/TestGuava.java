package singularity.benchmarks;


import org.apache.commons.lang3.tuple.ImmutablePair;
import patbench.guava.common.collect.ImmutableBiMap;
import patbench.guava.common.collect.ImmutableMultimap;
import patbench.guava.common.collect.ImmutableMultiset;
import patbench.guava.common.collect.ImmutableSet;
import scala.Tuple2;
import scala.collection.Seq;

import java.util.ArrayList;
import java.util.Map;

public class TestGuava {
    public static <A, B> ArrayList<ImmutablePair<A,B>> toArrayListPair(Seq<Tuple2<A,B>> pairs) {
        ArrayList<ImmutablePair<A,B>> list = new ArrayList<>();
        pairs.foreach(p -> list.add(new ImmutablePair<>(p._1, p._2)));
        return list;
    }

    public static <A> ArrayList<A> seqToArrayList(Seq<A> seq) {
        ArrayList<A> list = new ArrayList<>();
        seq.foreach(list::add);
        return list;
    }

    public static <A, B> ImmutableBiMap<A,B> arrayListToBiMap(ArrayList<ImmutablePair<A,B>> list){
        return ImmutableBiMap.copyOf(list);
    }

    public static <A> ImmutableSet<A> arrayListToSet(ArrayList<A> list){
        return ImmutableSet.copyOf(list);
    }

    public static <A> ImmutableMultiset<A> arrayListToMultiset(ArrayList<A> list) {
        return ImmutableMultiset.copyOf(list);
    }
}
