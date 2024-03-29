package io.github.greyp9.arwo.app.local.fs.data;

import io.github.greyp9.arwo.app.local.fs.core.LFSRequest;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.model.ExceptionModel;
import io.github.greyp9.arwo.core.file.FileU;
import io.github.greyp9.arwo.core.file.meta.FileMetaData;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.io.StreamU;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.util.Collection;
import java.util.TreeSet;

public class LFSDataSource {
    private final LFSRequest request;
    private final File folderRoot;

    public LFSDataSource(final LFSRequest request, final File folderRoot) {
        this.request = request;
        this.folderRoot = folderRoot;
    }

    public final File getFile(final String path) {
        return new File(folderRoot, path);
    }

    public final File[] listFiles(final String path, final boolean recurse) throws IOException {
        final Collection<File> files = new TreeSet<>();
        final File folder = FileU.getCanonicalFolder(new File(folderRoot, path));
        listFilesR(folder, files, recurse);
        return files.toArray(new File[files.size()]);
    }

    private void listFilesR(final File folder, final Collection<File> filesAll, final boolean recurse) {
        final File[] files = FileU.listFiles(folder);
        for (File file : files) {
            filesAll.add(file);
            if (recurse && file.isDirectory()) {
                listFilesR(file, filesAll, true);
            }
        }
    }

    public final void createDirectory(final String path) {
        FileU.ensureFolder(new File(folderRoot, path));
    }

    public final void deleteDirectory(final String path) throws IOException {
        delete(path);
    }

    public final void move(final String to, final String from) throws IOException {
        final IOException e = new IOException(Integer.toString(HttpURLConnection.HTTP_NOT_IMPLEMENTED));
        new ExceptionModel(request.getAlerts()).service(e, Alert.Severity.ERR);
    }

    public final void delete(final String path) throws IOException {
        final File file = new File(folderRoot, path);
        FileU.delete(file);
    }

    public final File[] lsSymlink(final String path) {
        final Collection<File> files = new TreeSet<>();
        File fileIt = new File(path);
        files.add(fileIt);
        while ((fileIt != null) && (FileU.isLink(fileIt))) {
            fileIt = FileU.getCanonicalFile(fileIt);
            files.add(fileIt);
        }
        return files.toArray(new File[files.size()]);
    }

    public final File exists(final String path) {
        final File file = new File(folderRoot, path);
        return (file.exists() ? file : null);
    }

    public final MetaFile read(final String path) throws IOException {
        long lastModified = 0L;
        byte[] bytes = new byte[0];
        try {
            final File fileTarget = new File(folderRoot, path);
            lastModified = fileTarget.lastModified();
            bytes = StreamU.read(fileTarget);
        } catch (IOException e) {
            new ExceptionModel(request.getAlerts()).service(e, Alert.Severity.ERR);
        }
        final FileMetaData metaData = new FileMetaData(path, bytes.length, lastModified, false);
        return new MetaFile(metaData, null, new ByteArrayInputStream(bytes));
    }

    public final void write(final byte[] bytes, final String folder, final String filename) throws IOException {
        try {
            final File folderTarget = new File(folderRoot, folder);
            final File fileTarget = new File(folderTarget, filename);
            StreamU.write(fileTarget, bytes);
        } catch (IOException e) {
            new ExceptionModel(request.getAlerts()).service(e, Alert.Severity.ERR);
        }
    }
}
