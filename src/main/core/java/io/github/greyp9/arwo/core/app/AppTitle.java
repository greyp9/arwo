package io.github.greyp9.arwo.core.app;

import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.value.Value;

public class AppTitle {
    private final String[] tokens;

    public AppTitle(final String... tokens) {
        this.tokens = tokens;
    }

    public final String getText() {
        return Value.join(" - ", (Object[]) tokens);  // i18n revisit
    }

    public static final class Factory {
        private Factory() {
        }

        public static AppTitle getHostLabel(
                final ServletHttpRequest httpRequest, final Bundle bundle) {
            final String host = httpRequest.getHttpRequest().getHeader(Http.Header.HOST);
            return new AppTitle(getLabel(httpRequest, bundle), host);
        }

        public static AppTitle getHostLabel(
                final ServletHttpRequest httpRequest, final String label, final String context) {
            final String host = httpRequest.getHttpRequest().getHeader(Http.Header.HOST);
            return new AppTitle(context, label, host);
        }

        public static AppTitle getHostLabel(
                final ServletHttpRequest httpRequest, final Bundle bundle, final String context) {
            final String host = httpRequest.getHttpRequest().getHeader(Http.Header.HOST);
            return new AppTitle(context, getLabel(httpRequest, bundle), host);
        }

        public static AppTitle getResourceLabel(final ServletHttpRequest httpRequest, final Bundle bundle,
                                                final String resource, final String... facets) {
            final String host = httpRequest.getHttpRequest().getHeader(Http.Header.HOST);
            return new AppTitle(resource, getLabel(httpRequest, bundle), bundle.localize(facets), host);
        }

        private static String getLabel(final ServletHttpRequest httpRequest, final Bundle bundle) {
            final String key = Value.join(Http.Token.DOT, Html.HTML_TOKEN, Html.TITLE, httpRequest.getServletPath());
            return bundle.getString(key);
        }
    }
}
