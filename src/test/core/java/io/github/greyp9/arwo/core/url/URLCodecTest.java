package io.github.greyp9.arwo.core.url;

import junit.framework.TestCase;
import org.junit.Assert;

public class URLCodecTest extends TestCase {

    public void testEncodePath() throws Exception {
        final String pathA = "/a b/c d/e f.txt";
        final String pathEncoded = URLCodec.encodePath(pathA);
        Assert.assertEquals("/a%20b/c%20d/e%20f.txt", pathEncoded);
        final String pathZ = URLCodec.decode(pathEncoded);
        Assert.assertEquals(pathA, pathZ);
    }
}
