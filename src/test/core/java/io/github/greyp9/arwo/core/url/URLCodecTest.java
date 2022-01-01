package io.github.greyp9.arwo.core.url;

import io.github.greyp9.arwo.core.vm.app.ManifestU;
import org.junit.Assert;
import org.junit.Test;

import java.io.File;
import java.net.JarURLConnection;
import java.net.URL;
import java.net.URLConnection;
import java.util.jar.JarFile;
import java.util.jar.Manifest;

public class URLCodecTest {

    @Test
    public void testEncodePath() throws Exception {
        final String pathA = "/a b/c d/e f.txt";
        final String pathEncoded = URLCodec.encodePath(pathA);
        Assert.assertEquals("/a%20b/c%20d/e%20f.txt", pathEncoded);
        final String pathZ = URLCodec.decode(pathEncoded);
        Assert.assertEquals(pathA, pathZ);
    }

    private void checkManifestJar(URL url) throws Exception {
        final File file = URLCodec.toFile(url);
        final JarFile jarFile = new JarFile(file);
        final Manifest manifest = jarFile.getManifest();
        Assert.assertEquals("1.0", ManifestU.getAttribute(manifest, "Manifest-Version"));
        Assert.assertNotNull(ManifestU.getImplementationBuild(manifest));
    }

    private void checkManifestConnection(URL url) throws Exception {
        final URLConnection urlConnection = url.openConnection();
        final JarURLConnection jarURLConnection = (JarURLConnection) urlConnection;
        final Manifest manifest = jarURLConnection.getManifest();
        Assert.assertEquals("1.0", ManifestU.getAttribute(manifest, "Manifest-Version"));
        Assert.assertNotNull(ManifestU.getImplementationBuild(manifest));
    }
}
