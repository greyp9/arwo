package io.github.greyp9.arwo.core.file.find;

import io.github.greyp9.arwo.core.file.FileU;

import java.io.File;
import java.io.FileFilter;
import java.util.Arrays;
import java.util.Collection;
import java.util.TreeSet;
import java.util.regex.Pattern;

public class FindInFolderQuery {
    private final FileFilter filterFile;
    private final FileFilter filterFolder;
    private final Collection<File> found;

    public FindInFolderQuery(final File folder, final Pattern pattern, final boolean recurse) {
        this.filterFile = new WildcardFileFilter(pattern);
        this.filterFolder = (recurse ? new IsDirectoryFilter() : null);
        this.found = new TreeSet<File>();
        findFiles(folder);
    }

    public FindInFolderQuery(final File folder, final String pattern, final boolean recurse) {
        this.filterFile = new WildcardFileFilter(pattern);
        this.filterFolder = (recurse ? new IsDirectoryFilter() : null);
        this.found = new TreeSet<File>();
        findFiles(folder);
    }

    public final Collection<File> getFound() {
        return found;
    }

    private void findFiles(final File folder) {
        found.addAll(Arrays.asList(FileU.listFiles(folder, filterFile)));
        if (filterFolder != null) {
            findFilesChildren(folder);
        }
    }

    private void findFilesChildren(final File folder) {
        final File[] folders = FileU.listFiles(folder, filterFolder);
        for (final File folderIt : folders) {
            findFiles(folderIt);
        }
    }

/*
    public static void filterOut(final Collection<File> files, final String pattern) {
        final FileFilter filter = new WildcardFileFilter(pattern);
        for (final Iterator<File> iterator = files.iterator(); iterator.hasNext(); iterator.getClass()) {
            final File file = iterator.next();
            if (filter.accept(file)) {
                iterator.remove();
            }
        }
    }
*/
}
