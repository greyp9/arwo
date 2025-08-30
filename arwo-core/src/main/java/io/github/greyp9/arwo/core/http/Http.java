package io.github.greyp9.arwo.core.http;

// i18nf
public final class Http {

    private Http() {
    }

    public static class Protocol {
        public static final String HTTP = "http";
        public static final String HTTPS = "https";
    }

    public static class Token {
        public static final String AMP = "&";
        public static final String BACKSLASH = "\\";
        public static final String COLON = ":";
        public static final String COMMA = ",";
        public static final String EQUALS = "=";
        public static final String HYPHEN = "-";
        public static final String QUERY = "?";
        public static final String QUOTE = "\"";
        public static final String SEMICOLON = ";";
        public static final String SLASH = "/";
        public static final String SPACE = " ";
        public static final String DOT = ".";
        public static final String CRLF = "\r\n";
        public static final String LF = "\n";
    }

    public static class Header {
        public static final String ACCEPT = "Accept";
        public static final String ACCEPT_ENCODING = "Accept-Encoding";
        public static final String AUTHORIZATION = "Authorization";
        public static final String CONTENT_ENCODING = "Content-Encoding";
        public static final String CONTENT_LENGTH = "Content-Length";
        public static final String CONTENT_TYPE = "Content-Type";
        public static final String COOKIE = "Cookie";
        public static final String EXPIRES = "Expires";
        public static final String GZIP = "gzip";
        public static final String HOST = "Host";
        public static final String LAST_MODIFIED = "Last-Modified";
        public static final String LOCATION = "Location";
        public static final String USER_AGENT = "User-Agent";
        public static final String WWW_AUTHENTICATE = "WWW-Authenticate";
        public static final String X_XSRF_TOKEN = "x-xsrf-token";
    }

    public static class Realm {
        public static final String BASIC = "Basic";
        public static final String BASIC_REALM = "Basic Realm=\"%s\"";
        public static final String CERTIFICATE = "Certificate";
    }

    public static class Mime {
        public static final String TEXT_HTML_UTF8 = "text/html; charset='UTF-8'";
        public static final String TEXT_PLAIN_UTF8 = "text/plain; charset='UTF-8'";
        public static final String TEXT_PLAIN_UTF16 = "text/plain; charset='UTF-16'";
        public static final String TEXT_XML_UTF8 = "text/xml; charset='UTF-8'";

        public static final String APP_CERT = "application/pkix-cert";
        public static final String APP_JSON = "application/json";
        public static final String APP_ZIP = "application/zip";
        public static final String APP_GZIP = "application/gzip";
        public static final String APP_TGZ = "application/tar+gzip";

        public static final String FORM_URL_ENCODED = "application/x-www-form-urlencoded";
        public static final String FORM_MULTIPART = "multipart/form-data";
    }

    public static class Method {
        public static final String GET = "GET";
        public static final String POST = "POST";
        public static final String PUT = "PUT";
        public static final String DELETE = "DELETE";
    }
}
