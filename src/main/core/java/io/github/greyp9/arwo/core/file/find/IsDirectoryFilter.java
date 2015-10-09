package io.github.greyp9.arwo.core.file.find;

import java.io.File;
import java.io.FileFilter;

public class IsDirectoryFilter implements FileFilter {

    public final boolean accept(final File file) {
        return file.isDirectory();
    }
}
