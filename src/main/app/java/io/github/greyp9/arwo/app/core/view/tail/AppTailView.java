package io.github.greyp9.arwo.app.core.view.tail;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.config.Preferences;
import io.github.greyp9.arwo.core.date.HttpDateU;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.ByteU;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.lang.NumberU;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import org.w3c.dom.Element;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.util.Date;

public class AppTailView {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;

    public AppTailView(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.httpRequest = httpRequest;
        this.userState = userState;
    }

    public final HttpResponse addContentTo(
            final Element html, final MetaFile metaFile, final Bundle bundle) throws IOException {
        final Preferences preferences = new Preferences(userState.getConfig());
        final String settingTailSize = preferences.getSetting("/app:view/app:tailSize");
        final int tailSize = NumberU.toInt(settingTailSize, Const.PAGE_TAIL_VIEW);

        httpRequest.getClass();
        html.getClass();
        bundle.getClass();

        // get last bytes of resource
        final byte[] bytes = StreamU.read(metaFile.getBytes());
        final int offset = Math.max(0, bytes.length - tailSize);
        final int length = bytes.length - offset;
        final byte[] bytesTail = ByteU.extract(bytes, offset, length);
        final String lastModified = HttpDateU.toHttpZ(new Date(metaFile.getMetaData().getLastModified()));
        final NameTypeValues headers = new NameTypeValues(new NameTypeValue(Http.Header.LAST_MODIFIED, lastModified));
        headers.add(new NameTypeValue(Http.Header.CONTENT_TYPE, Http.Mime.TEXT_PLAIN_UTF8));
        return new HttpResponse(HttpURLConnection.HTTP_OK, headers, new ByteArrayInputStream(bytesTail));
    }

    private static class Const {
        private static final int PAGE_TAIL_VIEW = 4096;
    }
}
