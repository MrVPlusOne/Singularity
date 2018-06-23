package singularity.weave_original;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Random;
// import org.slf4j.Logger;
// import org.slf4j.LoggerFactory;

public class Weave {
    // private static final Logger logger = LoggerFactory.getLogger(Weave.class);
    public static final int MOST_STRANDS = 27;
    public static final int OPTIMUM_STRANDS = 4;
    public static final int MOST_LENGTH = 52;
    static final int OPTIMUM_LENGTH = 5;
    private String intersections = "";
    private int numStrands;
    private char optimumBaseChar;
    private int penalty;
    public static Random random = new Random(0);

    public Weave(String intersections, int numStrands) {
        intersections = intersections.replace(" ", "");
        if (numStrands > 27 || numStrands < 4) {
            throw new IllegalArgumentException("numStrands must be between 4 and 27");
        }
        if (intersections.length() > 52) {
            throw new IllegalArgumentException("Braid representation length must be no more than 52");
        }
        this.intersections = intersections;
        this.numStrands = numStrands;
        this.penalty = this.calculatePenalty();
        this.optimumBaseChar = (char)(122 - numStrands + 2);
        char[] charArray = intersections.toCharArray();
        for (int j = 0; j < charArray.length; ++j) {
            char c = charArray[j];
            if (Character.isAlphabetic(c) && Character.toLowerCase(c) >= this.optimumBaseChar) continue;
            throw new IllegalArgumentException("Character " + c + " not allowed in a braid of " + numStrands + " strands");
        }
    }

    public static Weave grabRandomWeave(int numStrands, long seed) {
        Random random = new Random(seed);
        char[] baseLetters = new char[numStrands - 1];
        int p = 0;
        while (p < numStrands - 1) {
            while (p < numStrands - 1 && Math.random() < 0.5) {
                baseLetters[p] = (char)(122 - p);
                ++p;
            }
        }
        String weave = "";
        int length = random.nextInt(48) + 5;
        boolean a = false;
        for (int k = 0; k < length; ++k) {
            char ch;
            int index = random.nextInt(numStrands - 2);
            char charToAdd = ch = baseLetters[index];
            boolean invert = random.nextBoolean();
            if (invert) {
                charToAdd = Weave.grabInverse(Character.valueOf(ch)).charValue();
            }
            weave = weave + charToAdd;
        }
        return new Weave(weave, numStrands);
    }

    public static Weave getRandomBraid(int numStrands, int length) {
        char[] baseLetters = new char[numStrands - 1];
        for (int c = 0; c < numStrands - 1; ++c) {
            baseLetters[c] = (char)(122 - c);
        }
        String weave = "";
        boolean a = false;
        for (int i = 0; i < length; ++i) {
            char ch;
            int index = random.nextInt(numStrands - 2);
            char charToAdd = ch = baseLetters[index];
            boolean invert = random.nextBoolean();
            if (invert) {
                charToAdd = Weave.grabInverse(Character.valueOf(ch)).charValue();
            }
            weave = weave + charToAdd;
        }
        return new Weave(weave, numStrands);
    }

    public String pullIntersections() {
        return this.intersections;
    }

    public int grabNumStrands() {
        return this.numStrands;
    }

    public void concatenate(Weave other) {
        if (this.numStrands != other.numStrands) {
            throw new IllegalArgumentException("Can't concatenate braids with a different number of strands.");
        }
        this.intersections = this.intersections + other.pullIntersections();
        this.penalty = other.calculatePenalty();
    }

    public boolean isEquivalent(Weave other) {
        if (this.numStrands != other.numStrands) {
            return false;
        }
        Weave b = other.fetchInverse();
        b.concatenate(this);
        return b.isUnity();
    }

    public boolean isUnity() {
        this.reduceCompletely();
        return this.intersections.isEmpty();
    }

    public boolean isNormalized() {
        if (this.intersections.isEmpty()) {
            return true;
        }
        char lowest = '\uffff';
        char base = (char)(122 - this.penalty + 1);
        char[] charArray = this.intersections.toCharArray();
        for (int p = 0; p < charArray.length; ++p) {
            char c = charArray[p];
            char candidate = Character.toLowerCase(c);
            if (candidate < Character.toLowerCase(lowest)) {
                lowest = c;
            }
            if (candidate >= Character.toLowerCase(base)) continue;
            base = c;
        }
        if (this.intersections.contains(Character.toString(Weave.grabInverse(Character.valueOf(lowest)).charValue()))) {
            return false;
        }
        if (this.intersections.contains(Character.toString(Weave.grabInverse(Character.valueOf(base)).charValue()))) {
            return false;
        }
        return true;
    }

    public Weave fetchInverse() {
        String inverseIntersections = "";
        for (int i = this.intersections.length() - 1; i >= 0; --i) {
            Character c = Character.valueOf(this.intersections.charAt(i));
            inverseIntersections = inverseIntersections + Weave.grabInverse(c);
        }
        return new Weave(inverseIntersections, this.numStrands);
    }

    public void reduceCompletely() {
        this.freeReduce();
        while (!this.isNormalized()) {
            this.reduceCompletelyEntity();
        }
    }

    private void reduceCompletelyEntity() {
        this.reduceOnce();
        this.freeReduce();
    }

    public boolean makeRandomModification(int numAttempts) {
        return this.makeRandomModification(numAttempts, null);
    }

    public boolean makeRandomModification(int numAttempts, Long seed) {
        if (seed != null) {
            random.setSeed(seed);
        }
        for (int i = 0; i < numAttempts; ++i) {
            int j = random.nextInt(4);
            boolean success = false;
            int index;
            switch (j) {
                case 0: {
                    index = random.nextInt(this.intersections.length());
                    success = this.growOneToThree(index);
                    break;
                }
                case 1: {
                    index = random.nextInt(this.intersections.length());
                    success = this.growOneToFive(index);
                    break;
                }
                case 2: {
                    success = this.swapRandom();
                    break;
                }
                case 3: {
                    success = this.flipRandomTriple(null);
                }
            }
            if (!success) continue;
            return true;
        }
        return false;
    }

    public boolean growOneToThree(int index) {
        char intersection;
        if (this.intersections.length() + 2 > 52) {
            System.out.println("Representation would be too long; not expanding further");
            return false;
        }
        if (index < 0 || index >= this.intersections.length()) {
            return false;
        }
        char ch = this.intersections.charAt(index);
        char chBase = Character.toLowerCase(ch);
        if (chBase > this.optimumBaseChar + '\u0001' && random.nextBoolean() || chBase > 'x') {
            int optimum = 2;
            int most = chBase - this.optimumBaseChar - 1;
            if (optimum > most) {
                return false;
            }
            int delta = random.nextInt(most - optimum + 1) + optimum;
            intersection = (char)(ch - delta);
        } else if (chBase < 'y') {
            int optimum = 2;
            int most = 122 - chBase;
            if (optimum > most) {
                return false;
            }
            int delta = random.nextInt(most - optimum + 1) + optimum;
            intersection = (char)(ch + delta);
        } else {
            return false;
        }
        // logger.debug("Expand 1-3 of index={} ch={} crossing={}", new Object[]{index, Character.valueOf(ch), Character.valueOf(intersection)});
        this.insertInIntersections(index, index + 1, "" + intersection + ch + Weave.grabInverse(Character.valueOf(intersection)));
        return true;
    }

    public boolean growOneToFive(int index) {
        if (this.intersections.length() + 4 > 52) {
            System.out.println("Representation would be too long; not expanding further");
            return false;
        }
        if (index < 0 || index >= this.intersections.length()) {
            return false;
        }
        char ch = this.intersections.charAt(index);
        char chBase = Character.toLowerCase(ch);
        if (chBase == 'z') {
            return false;
        }
        char next = (char)(ch + '\u0001');
        // logger.debug("Expand 1-5 of index={} ch={} next={}", new Object[]{index, Character.valueOf(ch), Character.valueOf(next)});
        this.insertInIntersections(index, index + 1, "" + next + ch + next + Weave.grabInverse(Character.valueOf(ch)) + Weave.grabInverse(Character.valueOf(next)));
        return true;
    }

    public boolean swapRandom() {
        int index = random.nextInt(this.intersections.length() - 1);
        return this.swap(index);
    }

    public boolean swap(int index) {
        if (index > this.intersections.length() - 2) {
            return false;
        }
        if (index < 0) {
            return false;
        }
        char curr = this.intersections.charAt(index);
        char next = this.intersections.charAt(index + 1);
        // logger.debug("Swap of index={} curr={} next={}", new Object[]{index, Character.valueOf(curr), Character.valueOf(next)});
        if (Math.abs(Character.toLowerCase(curr) - Character.toLowerCase(next)) < 2) {
            return false;
        }
        this.insertInIntersections(index, index + 2, "" + next + curr);
        return true;
    }

    public boolean flipRandomTriple(Long seed) {
        List<Integer> indices;
        if (seed != null) {
            random.setSeed(seed);
        }
        if ((indices = this.encounterTriples()).isEmpty()) {
            return false;
        }
        // logger.debug("Triples of \"{}\" found at indices={}", (Object)this.intersections, indices);
        int index = random.nextInt(indices.size());
        int triplePosition = indices.get(index);
        char c1 = this.intersections.charAt(triplePosition + 1);
        char c2 = this.intersections.charAt(triplePosition);
        this.insertInIntersections(triplePosition, triplePosition + 3, "" + c1 + c2 + c1);
        return true;
    }

    public List<Integer> encounterTriples() {
        ArrayList<Integer> indices = new ArrayList<Integer>();
        for (int p = 0; p < this.intersections.length() - 2; ++p) {
            if (Math.abs(this.intersections.charAt(p) - this.intersections.charAt(p + 1)) != 1 || this.intersections.charAt(p) != this.intersections.charAt(p + 2)) continue;
            indices.add(p);
        }
        return indices;
    }

    private void insertInIntersections(int i, int j, String portion) {
        StringBuilder builder = new StringBuilder();
        builder.append(this.intersections.substring(0, i));
        builder.append(portion);
        builder.append(this.intersections.substring(j));
        this.intersections = builder.toString();
    }

    public void freeReduce() {
        boolean normalized = false;
        for (int b = 0; b < this.intersections.length() - 1; ++b) {
            Character second;
            Character first = Character.valueOf(this.intersections.charAt(b));
            if (first == (second = Character.valueOf(this.intersections.charAt(b + 1))) || Character.toLowerCase(first.charValue()) != Character.toLowerCase(second.charValue())) continue;
            this.intersections = this.intersections.substring(0, b) + this.intersections.substring(b + 2);
            --b;
            normalized = true;
        }
        if (normalized) {
            this.freeReduce();
        }
        // logger.info("After free reduce: " + this.intersections);
    }

    private IndexPair grabNextPortion() {
        int startIndex = -1;
        char[] charArray = this.intersections.toCharArray();
        for (int i1 = 0; i1 < charArray.length; ++i1) {
            int i;
            Character c = Character.valueOf(charArray[i1]);
            int endIndex = this.intersections.indexOf(Weave.grabInverse(c).charValue(), ++startIndex);
            if (endIndex < 0 || (i = this.intersections.indexOf(c.charValue(), startIndex + 1)) > 0 && i < endIndex) continue;
            if (Character.isAlphabetic(c.charValue() - '\u0001')) {
                i = this.intersections.indexOf(c.charValue() - '\u0001', startIndex);
                if (i > 0 && i < endIndex || (i = this.intersections.indexOf(Weave.grabInverse(Character.valueOf((char)(c.charValue() - '\u0001'))).charValue(), startIndex)) > 0 && i < endIndex) {
                    continue;
                }
            } else if (endIndex - startIndex > 104) continue;
            if (Character.isAlphabetic(c.charValue() + '\u0001')) {
                i = this.intersections.indexOf(c.charValue() + '\u0001', startIndex);
                int j = this.intersections.indexOf(Weave.grabInverse(Character.valueOf((char)(c.charValue() + '\u0001'))).charValue(), startIndex);
                if (i > 0 && j > 0 && i < endIndex && j < endIndex) continue;
            }
            return new IndexPair(startIndex, endIndex);
        }
        return null;
    }

    private void reduceOnce() {
        IndexPair indices = this.grabNextPortion();
        if (indices == null) {
            return;
        }
        this.removePortion(indices);
        // logger.info("After reduction step: " + this.intersections);
    }

    private void removePortion(IndexPair indices) {
        String portion = this.intersections.substring(indices.a, indices.b + 1);
        char c = this.intersections.charAt(indices.a);
        char cInv = this.intersections.charAt(indices.b);
        String cStr = String.valueOf(c);
        String cInvStr = String.valueOf(cInv);
        String cPlusStr = String.valueOf((char)(c + '\u0001'));
        String cInvPlusStr = String.valueOf((char)(cInv + '\u0001'));
        String newPortion = "";
        char[] charArray = portion.toCharArray();
        for (int k = 0; k < charArray.length; ++k) {
            char ch = charArray[k];
            if (ch == c || ch == cInv) continue;
            newPortion = ch == cInv + '\u0001' ? newPortion + cInvPlusStr + cInvStr + cPlusStr : (ch == c + '\u0001' ? newPortion + cInvPlusStr + cStr + cPlusStr : newPortion + ch);
        }
        this.insertInIntersections(indices.a, indices.b + 1, newPortion);
    }

    private static Character grabInverse(Character c) {
        if (Character.isLowerCase(c.charValue())) {
            return Character.valueOf(Character.toUpperCase(c.charValue()));
        }
        if (Character.isUpperCase(c.charValue())) {
            return Character.valueOf(Character.toLowerCase(c.charValue()));
        }
        throw new IllegalArgumentException("Crossing characters must be alphabetical " + c);
    }

    public int calculatePenalty() {
        HashMap<Character, Integer> countsMap = new HashMap<Character, Integer>();
        char[] charArray = this.intersections.toCharArray();
        for (int k = 0; k < charArray.length; ++k) {
            char c = charArray[k];
            if (countsMap.containsKey(Character.valueOf(c))) {
                countsMap.put(Character.valueOf(c), (Integer)countsMap.get(Character.valueOf(c)) + 1);
                continue;
            }
            countsMap.put(Character.valueOf(c), 1);
        }
        Collection counts = countsMap.values();
        if (counts.size() < 26) {
            return 0;
        }
        int optimum = (Integer)Collections.min(counts, null);
        int most = (Integer)Collections.max(counts, null);
        // logger.debug("Compute cost min={} max={}", (Object)optimum, (Object)most);
        return most - optimum;
    }

    public String toString() {
        return this.intersections;
    }

    private static class IndexPair {
        int a;
        int b;

        IndexPair(int a, int b) {
            this.a = a;
            this.b = b;
        }
    }

    private static class IntCompare
    implements Comparator<Integer> {
        private IntCompare() {
        }

        @Override
        public int compare(Integer i1, Integer i2) {
            return 0;
        }
    }

}

