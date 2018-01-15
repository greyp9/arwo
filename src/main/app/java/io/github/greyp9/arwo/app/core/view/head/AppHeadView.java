package io.github.greyp9.arwo.app.core.view.head;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.date.HttpDateU;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.ByteU;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import org.w3c.dom.Element;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.util.Date;

public class AppHeadView {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;

    public AppHeadView(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.httpRequest = httpRequest;
        this.userState = userState;
    }

    public final HttpResponse addContentTo(
            final Element html, final MetaFile metaFile, final Bundle bundle) throws IOException {
        httpRequest.getClass();
        userState.getClass();
        html.getClass();
        bundle.getClass();
        // get first bytes of resource
        final byte[] bytes = StreamU.read(metaFile.getBytes());
        final int offset = 0;
        final int length = Math.min(bytes.length, Const.PAGE_HEAD_VIEW);
        final byte[] bytesHead = ByteU.extract(bytes, offset, length);
        final String lastModified = HttpDateU.toHttpZ(new Date(metaFile.getMetaData().getLastModified()));
        final NameTypeValues headers = new NameTypeValues(new NameTypeValue(Http.Header.LAST_MODIFIED, lastModified));
        headers.add(new NameTypeValue(Http.Header.CONTENT_TYPE, Http.Mime.TEXT_PLAIN_UTF8));
        return new HttpResponse(HttpURLConnection.HTTP_OK, headers, new ByteArrayInputStream(bytesHead));
    }

    private static class Const {
        private static final int PAGE_HEAD_VIEW = 4096;
    }
}
