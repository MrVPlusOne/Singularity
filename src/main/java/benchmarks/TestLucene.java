package benchmarks;

import patbench.lucene.analysis.Analyzer;
import patbench.lucene.analysis.TokenStream;
import patbench.lucene.analysis.tokenattributes.CharTermAttribute;

import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

public class TestLucene {
    public static List<String> tokenizeWithAnalyzer(Analyzer analyzer, String str) {
        List<String> result = new ArrayList<>();
        try (TokenStream stream  = analyzer.tokenStream(null, new StringReader(str))) {
            stream.reset();
            while (stream.incrementToken()) {
                result.add(stream.getAttribute(CharTermAttribute.class).toString());
            }
        } catch (IOException e) {
            // not thrown b/c we're using a string reader...
            throw new RuntimeException(e);
        }
        return result;
    }
}
