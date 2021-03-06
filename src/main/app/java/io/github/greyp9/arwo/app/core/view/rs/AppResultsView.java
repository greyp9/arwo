package io.github.greyp9.arwo.app.core.view.rs;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.mime.MimeMagic;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.result.op.Results;
import io.github.greyp9.arwo.core.result.view.ResultsView;
import io.github.greyp9.arwo.core.result.xml.ResultsReader;
import org.w3c.dom.Element;

import java.io.IOException;

public class AppResultsView {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;

    public AppResultsView(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.httpRequest = httpRequest;
        this.userState = userState;
    }

    public final HttpResponse addContentTo(final Element html, final MetaFile metaFile) throws IOException {
        HttpResponse httpResponse = null;
        final boolean isXML = new MimeMagic(metaFile.getBytes()).isXML();
        if (isXML) {
            final Results results = new ResultsReader().readFrom(metaFile);
            new ResultsView(results, userState.getResultsContext(httpRequest)).addContentTo(html);
        } else {
            // handle view of text data (non-XML) in this context
            httpResponse = HttpResponseU.to200(metaFile);
        }
        return httpResponse;
    }
}
