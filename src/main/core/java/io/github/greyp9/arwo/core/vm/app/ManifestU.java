package io.github.greyp9.arwo.core.vm.app;

import io.github.greyp9.arwo.core.url.URLCodec;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.jar.JarFile;
import java.util.jar.Manifest;

public final class ManifestU {

    private ManifestU() {
    }

    @SuppressWarnings("PMD.OnlyOneReturn")
    public static Manifest getManifestSafe(final Class<?> c) {
        try {
            return getManifest(c);
        } catch (IOException e) {
            return new Manifest();
        }
    }

    public static Manifest getManifest(final Class<?> c) throws IOException {
        final URL url = c.getProtectionDomain().getCodeSource().getLocation();
        final File file = URLCodec.toFile(url);
        return (file.isFile() ? getManifest(file) : new Manifest());
    }

    public static Manifest getManifest(final File file) throws IOException {
        final JarFile jarFile = new JarFile(file);
        try {
            return jarFile.getManifest();
        } finally {
            jarFile.close();
        }
    }

    public static String getAttribute(final Manifest manifest, final String name) {
        return manifest.getMainAttributes().getValue(name);
    }

    public static String getImplementationBuild(final Manifest manifest) {
        return getAttribute(manifest, Const.BUILD);
    }

    public static String getImplementationVersion(final Manifest manifest) {
        return getAttribute(manifest, Const.VERSION);
    }

    public static String getVersion(final Class<?> c) {
        final Manifest manifest = getManifestSafe(c);
        return String.format("[v%s %s]",
                getImplementationVersion(manifest),
                getImplementationBuild(manifest));
    }

    private static class Const {
        public static final String BUILD = "Implementation-Build";  // i18n
        public static final String VERSION = "Implementation-Version";  // i18n
    }
}
