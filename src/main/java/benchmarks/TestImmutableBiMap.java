package benchmarks;


import org.apache.commons.lang3.tuple.ImmutablePair;
import patbench.guava.common.collect.ImmutableBiMap;
import scala.Tuple2;
import scala.collection.Seq;

import java.util.ArrayList;
import java.util.Map;

public class TestImmutableBiMap {
    public static <A, B> ImmutableBiMap<A,B> makeMap(Seq<Tuple2<A,B>> pairs) {
        ArrayList<ImmutablePair<A,B>> list = new ArrayList<>();
        pairs.foreach(p -> list.add(new ImmutablePair(p._1, p._2)));
        return ImmutableBiMap.copyOf(list);
    }
}
