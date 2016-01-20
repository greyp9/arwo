package io.github.greyp9.arwo.core.result.io;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppRequest;
import io.github.greyp9.arwo.core.file.FileX;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.result.op.Results;
import io.github.greyp9.arwo.core.result.xml.ResultsWriter;
import io.github.greyp9.arwo.core.value.Value;

import java.io.File;
import java.io.IOException;

public class ResultsPersister {
    private final AppRequest request;

    public ResultsPersister(final AppRequest request) {
        this.request = request;
    }

    public final File getFile(final File folderRoot) {
        File file = null;
        final String persist = request.getHttpRequest().getHeader(App.Header.RESULT);
        if (persist != null) {
            final String servletPath = request.getHttpRequest().getServletPath();
            final String filename1 = Value.defaultOnEmpty(
                    new FileX(request.getHttpRequest().getPathInfo()).getFilename(), null);
            final String filename = Value.join(Http.Token.HYPHEN, persist, filename1);
            file = new File(PathU.toDir(folderRoot.getPath(), Const.RESULT, servletPath, filename));
        }
        return file;
    }

    public final void write(final File folderRoot, final byte[] bytes) throws IOException {
        final File file = getFile(folderRoot);
        if (file != null) {
            StreamU.writeMkdirs(file, bytes);
        }
    }

    public final void write(final File folderRoot, final Results results) throws IOException {
        final File file = getFile(folderRoot);
        if (file != null) {
            new ResultsWriter().writeTo(file, results);
        }
    }

    private static class Const {
        private static final String RESULT = "result";  // i18n
    }
}
