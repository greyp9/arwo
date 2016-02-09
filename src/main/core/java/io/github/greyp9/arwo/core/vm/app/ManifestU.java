package io.github.greyp9.arwo.core.vm.app;

import java.util.jar.Manifest;

public final class ManifestU {

    private ManifestU() {
    }

    @SuppressWarnings("PMD.OnlyOneReturn")
/*
    public static Manifest getManifestSafe(final Class<?> c) {
        try {
            return getManifest(c);
        } catch (IOException e) {
            return new Manifest();
        }
    }

    public static Manifest getManifest(final Class<?> c) throws IOException {
        Manifest manifest;
        final URL url = c.getProtectionDomain().getCodeSource().getLocation();
        if ("file".equals(url.getProtocol())) {  // i18n JRE
            final File file = URLCodec.toFile(url);
            manifest = (file.isFile() ? getManifest(file) : new Manifest());
        } else if ("jar".equals(url.getProtocol())) {  // i18n JRE
            final URLConnection urlConnection = url.openConnection();
            final JarURLConnection jarURLConnection = (JarURLConnection) urlConnection;
            manifest = jarURLConnection.getManifest();
        } else {
            manifest = new Manifest();
        }
        return manifest;
    }

    public static Manifest getManifest(final File file) throws IOException {
        final JarFile jarFile = new JarFile(file);
        try {
            return jarFile.getManifest();
        } finally {
            jarFile.close();
        }
    }
*/

    public static String getAttribute(final Manifest manifest, final String name) {
        return manifest.getMainAttributes().getValue(name);
    }

    public static String getImplementationBuild(final Manifest manifest) {
        return getAttribute(manifest, Const.BUILD);
    }

/*
    public static String getImplementationVersion(final Manifest manifest) {
        return getAttribute(manifest, Const.IMPL_VERSION);
    }

    public static String getVersion2(final Class<?> c) {
        final Manifest manifest = getManifestSafe(c);
        return String.format("[v%s %s]",
                getImplementationVersion(manifest),
                getImplementationBuild(manifest));
    }
*/

    public static String getImplementationVersion(final Class<?> c) {
        return String.format("[b%s]", c.getPackage().getImplementationVersion());
    }

    public static String getSpecificationVersion(final Class<?> c) {
        return String.format("[v%s]", c.getPackage().getSpecificationVersion());
    }

    private static class Const {
        public static final String BUILD = "Implementation-Build";  // i18n JRE
        //public static final String IMPL_VERSION = "Implementation-Version";  // i18n JRE
        //public static final String SPEC_VERSION = "Specification-Version";  // i18n JRE
    }
}
