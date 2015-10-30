package io.github.greyp9.arwo.core.http;

import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;

import java.net.HttpURLConnection;

public final class HttpResponseU {

    private HttpResponseU() {
    }

    public static HttpResponse toHttpResponse302(final String location) {
        final NameTypeValues headers = new NameTypeValues(new NameTypeValue(Http.Header.LOCATION, location));
        return new HttpResponse(HttpURLConnection.HTTP_MOVED_TEMP, headers, null);
    }
}
