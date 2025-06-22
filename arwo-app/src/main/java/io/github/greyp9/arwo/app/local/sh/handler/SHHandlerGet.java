package io.github.greyp9.arwo.app.local.sh.handler;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.local.sh.core.SHRequest;
import io.github.greyp9.arwo.app.local.sh.view.SHCommandView;
import io.github.greyp9.arwo.app.local.sh.view.SHSearchView;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.file.meta.FileMetaData;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpArguments;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.value.NameTypeValues;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;

public class SHHandlerGet {
    private final SHRequest request;
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;

    public SHHandlerGet(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.request = new SHRequest(httpRequest, userState);
        this.httpRequest = httpRequest;
        this.userState = userState;
    }

    public final HttpResponse doGetSafe() throws IOException {
        HttpResponse httpResponse;
        try {
            httpResponse = doGet();
        } catch (IOException e) {
            userState.getAlerts().add(new Alert(Alert.Severity.ERR, e.getMessage()));
            httpResponse = HttpResponseU.to500(e.getMessage());
        }
        return httpResponse;
    }

    private HttpResponse doGet() throws IOException {
        HttpResponse httpResponse;
        final String baseURI = httpRequest.getBaseURI();
        final String pathInfo = httpRequest.getPathInfo();
        final String query = httpRequest.getHttpRequest().getQuery();
        final NameTypeValues ntvQuery = HttpArguments.toArguments(query);
        final boolean isResults = (ntvQuery.getValue("results") != null);
        final boolean isPathInfo = (pathInfo != null);
        final boolean isTrailingSlash = (isPathInfo && (pathInfo.endsWith(Http.Token.SLASH)));
        final boolean isQuery = (query != null);
        final boolean isNoPathInfo = (!isPathInfo);
        final boolean isNoTrailingSlash = (!isTrailingSlash);
        if (isNoPathInfo) {
            httpResponse = HttpResponseU.to302(PathU.toDir(baseURI));
        } else if (isNoTrailingSlash) {
            httpResponse = HttpResponseU.to302(PathU.toDir(httpRequest.getURI()));
        } else if (isResults) {
            httpResponse = doGetResults();
        } else if (isQuery) {
            httpResponse = HttpResponseU.to302(httpRequest.getURI());
        } else if ("search".equals(request.getContext())) {
            httpResponse = new SHSearchView(request, userState).doGetResponse();
        } else {
            httpResponse = new SHCommandView(request, userState).doGetResponse();
        }
        return httpResponse;
    }

    private HttpResponse doGetResults() throws IOException {
        final String filename = String.format("%s.results", request.getScriptID());
        final File file = new File(userState.getLocal().getFolderResultLSH(), filename);
        final FileMetaData metaData = new FileMetaData(null, file.length(), file.lastModified(), false);
        final byte[] content = StreamU.read(file);
        final MetaFile metaFile = new MetaFile(metaData, Http.Mime.TEXT_PLAIN_UTF8, new ByteArrayInputStream(content));
        return HttpResponseU.to200(metaFile);
    }
}
