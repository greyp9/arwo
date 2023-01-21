package io.github.greyp9.arwo.core.httpclient;

import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xpath.XPathContext;
import io.github.greyp9.arwo.core.xpath.XPather;
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

    public String getCookieValue(final String cookieName) {
        final HttpCookie httpCookie = cookies.stream()
                .filter(c -> c.getName().equals(cookieName))
                .findFirst()
                .orElse(null);
        return httpCookie == null ? null : httpCookie.getValue();
    }

    public List<HttpCookie> getCookiesDomain(final String domain) {
        return cookies.stream()
                .filter(c -> domain.equals(c.getDomain()))
                .collect(Collectors.toList());
    }

    public String toHeader() {
        final List<String> cookieCrumbs = new ArrayList<>();
        for (final HttpCookie cookie : cookies) {
            cookieCrumbs.add(String.format("%s=%s", cookie.getName(), cookie.getValue()));
        }
        return Value.joinCollection("; ", cookieCrumbs);
    }

    public static CookieJar fromXml(final byte[] xml) throws IOException {
        final CookieJar cookieJar = new CookieJar();
        final Document document = DocumentU.toDocument(xml);
        final XPathContext context = new XPathContext();
        context.addMapping("k", "urn:arwo:cookie");
        final XPather xpather = new XPather(document, context);
        final List<Element> elements = xpather.getElements("/k:cookies/k:cookie");
        for (final Element element : elements) {
            final HttpCookie cookie = new HttpCookie(
                    ElementU.getAttribute(element, "name"),
                    ElementU.getAttribute(element, "value"));
            cookie.setComment(ElementU.getAttribute(element, "comment"));
            cookie.setDomain(ElementU.getAttribute(element, "domain"));
            cookie.setPath(ElementU.getAttribute(element, "path"));
            cookie.setSecure(Boolean.parseBoolean(ElementU.getAttribute(element, "secure")));
            cookie.setMaxAge(Value.defaultOnNull(ElementU.getAttribute(element, "maxAge"), -1));
            cookie.setVersion(Value.defaultOnNull(ElementU.getAttribute(element, "version"), 0));
            cookieJar.getCookies().add(cookie);
        }
        return cookieJar;
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

    public void update(final List<HttpCookie> httpCookies) {
        cookies.addAll(httpCookies);
    }

    public void update(final String context, final NameTypeValues headers) {
        for (final NameTypeValue header : headers) {
            final String value = header.getValueS();
            if ("Set-Cookie".equalsIgnoreCase(header.getName()) && (!Value.isEmpty(value))) {
                logger.fine(String.format("CONTEXT [%s] HEADER [%s]", context, value));
                final List<HttpCookie> httpCookies = HttpCookie.parse(value);
                for (HttpCookie httpCookie : httpCookies) {
                    logger.fine(String.format("CONTEXT [%s] COOKIE [%s][%s]",
                            context, httpCookie.getName(), httpCookie.getValue()));
                    update(context, httpCookie);
                }
            }
        }
    }

    private void update(final String context, final HttpCookie httpCookie) {
        if (httpCookie.getValue().isEmpty()) {
            logger.fine(String.format("CONTEXT [%s] COOKIE [%s] REMOVE", context, httpCookie.getName()));
            cookies.removeAll(cookies.stream()
                    .filter(c -> c.getName().equalsIgnoreCase(httpCookie.getName()))
                    .collect(Collectors.toSet()));
        } else if (!httpCookie.getSecure()) {
            logger.fine(String.format("CONTEXT [%s] COOKIE [%s] FILTER INSECURE", context, httpCookie.getName()));
        } else {
            logger.fine(String.format("CONTEXT [%s] COOKIE [%s] SET", context, httpCookie.getName()));
            cookies.removeAll(cookies.stream()
                    .filter(c -> c.getName().equalsIgnoreCase(httpCookie.getName()))
                    .collect(Collectors.toSet()));
            cookies.add(httpCookie);
        }
    }
}
