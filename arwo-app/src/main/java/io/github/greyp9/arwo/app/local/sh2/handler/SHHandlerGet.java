package io.github.greyp9.arwo.app.local.sh2.handler;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.local.sh2.view.SHHistoryView;
import io.github.greyp9.arwo.app.local.sh2.view.SHScriptView;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.resource.Pather;
import io.github.greyp9.arwo.core.value.Value;

import java.io.IOException;

public class SHHandlerGet {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;

    public SHHandlerGet(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.httpRequest = httpRequest;
        this.userState = userState;
    }

    public final HttpResponse doGet() throws IOException {
        final String pathInfo = httpRequest.getPathInfo();
        final boolean isPathInfo = (pathInfo != null);
        final boolean isNoPathInfo = (!isPathInfo);
        final boolean isTrailingSlash = (isPathInfo && (pathInfo.endsWith(Http.Token.SLASH)));
        final boolean isNoTrailingSlash = (!isTrailingSlash);
        final Pather patherContext = new Pather(httpRequest.getPathInfo());
        final Pather patherScriptID = new Pather(patherContext.getRight());
        final String context = patherContext.getLeftToken();
        final String scriptID = patherScriptID.getLeftToken();

        HttpResponse httpResponse;
        if (isNoPathInfo) {
            httpResponse = HttpResponseU.to302(PathU.toDir(httpRequest.getBaseURI()));
        } else if (isNoTrailingSlash) {
            httpResponse = HttpResponseU.to302(PathU.toDir(httpRequest.getURI()));
        } else if (!Value.isEmpty(scriptID)) {
            httpResponse = new SHScriptView(httpRequest, userState, scriptID).doGetResponse();
        } else {
            httpResponse = new SHHistoryView(httpRequest, userState, context).doGetResponse();
        }
        return httpResponse;
    }
}
