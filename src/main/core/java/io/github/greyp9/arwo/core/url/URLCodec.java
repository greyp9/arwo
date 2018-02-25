package io.github.greyp9.arwo.core.url;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.http.Http;

import java.io.File;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.util.StringTokenizer;

public final class URLCodec {

    private URLCodec() {
    }

    public static String encode(final String value) throws UnsupportedEncodingException {
        final String encode = URLEncoder.encode(value, UTF8Codec.Const.UTF8);
        return encode.replace("+", "%20");  // is this a bug in JRE?  // i18n JRE
    }

    public static String decode(final String value) throws UnsupportedEncodingException {
        return URLDecoder.decode(value, UTF8Codec.Const.UTF8);
    }

    @SuppressWarnings("PMD.OnlyOneReturn")
    public static String decodeSafe(final String value) {
        try {
            return decode(value);
        } catch (UnsupportedEncodingException e) {
            return value;
        }
    }

    public static URL resolve(final URL url, final String relativeResource) throws IOException {
        try {
            final URI uri = url.toURI();
            final URI resolve = uri.resolve(relativeResource);
            return resolve.toURL();
        } catch (MalformedURLException e) {
            throw new IOException(e);
        } catch (URISyntaxException e) {
            throw new IOException(e);
        }
    }

    public static File toFile(final URL url) throws IOException {
        try {
            return new File(url.toURI());
        } catch (URISyntaxException e) {
            throw new IOException(e);
        }
    }

    public static URL toURL(final String path) throws MalformedURLException {
        return ((path == null) ? null : new URL(path));
    }

    public static URL toURL(final File file) throws MalformedURLException {
        return ((file == null) ? null : file.toURI().toURL());  // NOPMD
    }

    public static URI toURI(final String path) throws IOException {
        try {
            return ((path == null) ? null : new URI(path));
        } catch (URISyntaxException e) {
            throw new IOException(e);
        }
    }

    public static String toExternalForm(final URL url) {
        return ((url == null) ? null : url.toExternalForm());
    }

    public static String encodePath(final String path) throws UnsupportedEncodingException {
        final StringBuilder buffer = new StringBuilder();
        final StringTokenizer tokenizer = new StringTokenizer(path, Http.Token.SLASH, true);
        while (tokenizer.hasMoreTokens()) {
            final String token = tokenizer.nextToken();
            final boolean isSlash = Http.Token.SLASH.equals(token);
            buffer.append(isSlash ? token : encode(token));
        }
        return buffer.toString();
    }
}
