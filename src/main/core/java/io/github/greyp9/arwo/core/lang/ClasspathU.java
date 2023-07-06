package io.github.greyp9.arwo.core.lang;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;

public final class ClasspathU {

    private ClasspathU() {
    }

    public static Collection<File> getClassPath() {
        final Collection<File> files = new ArrayList<>();
        final String classPath = System.getProperty("java.class.path");
        final String pathSeparator = System.getProperty("path.separator");
        final String[] pathElements = classPath.split(pathSeparator);
        for (final String pathElement : pathElements) {
            files.add(new File(pathElement));
        }
        return files;
    }
}
