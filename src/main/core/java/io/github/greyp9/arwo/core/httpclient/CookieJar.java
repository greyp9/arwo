package io.github.greyp9.arwo.core.httpclient;

import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.IOException;
import java.net.HttpCookie;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import java.util.stream.Collectors;

public final class CookieJar {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final List<HttpCookie> cookies;

    public CookieJar() {
        this.cookies = new ArrayList<>();
    }

    public List<HttpCookie> getCookies() {
        return cookies;
    }

    public String toHeader() {
        final List<String> cookieCrumbs = new ArrayList<>();
        for (final HttpCookie cookie : cookies) {
            cookieCrumbs.add(String.format("%s=%s", cookie.getName(), cookie.getValue()));
        }
        return Value.joinCollection("; ", cookieCrumbs);
    }

    public byte[] toXml() throws IOException {
        final Document document = DocumentU.createDocument("cookies", "urn:arwo:cookie");
        final Element elementCookies = document.getDocumentElement();
        for (final HttpCookie cookie : cookies) {
            ElementU.addElement(elementCookies, "cookie", null, NTV.create(
                    "name", cookie.getName(),
                    "value", cookie.getValue(),
                    "comment", cookie.getComment(),
                    "domain", cookie.getDomain(),
                    "maxAge", Long.toString(cookie.getMaxAge()),
                    "path", cookie.getPath(),
                    "secure", Boolean.toString(cookie.getSecure()),
                    "version", Integer.toString(cookie.getVersion())));
        }
        return DocumentU.toXml(document);
    }

    public void update(final NameTypeValues headers) {
        for (final NameTypeValue header : headers) {
            final String value = header.getValueS();
            if ("Set-Cookie".equalsIgnoreCase(header.getName()) && (!Value.isEmpty(value))) {
                final List<HttpCookie> httpCookies = HttpCookie.parse(value);
                for (HttpCookie httpCookie : httpCookies) {
                    update(httpCookie);
                }
            }
        }
    }

    private void update(final HttpCookie httpCookie) {
        if (httpCookie.getValue().isEmpty()) {
            logger.fine("REMOVE COOKIE");
            cookies.removeAll(cookies.stream()
                    .filter(c -> c.getName().equalsIgnoreCase(httpCookie.getName()))
                    .collect(Collectors.toSet()));
        } else if (!httpCookie.getSecure()) {
            logger.fine("FILTER OUT INSECURE COOKIE");
        } else {
            cookies.removeAll(cookies.stream()
                    .filter(c -> c.getName().equalsIgnoreCase(httpCookie.getName()))
                    .collect(Collectors.toSet()));
            cookies.add(httpCookie);
        }
    }
}
