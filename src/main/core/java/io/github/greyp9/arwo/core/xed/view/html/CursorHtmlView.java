package io.github.greyp9.arwo.core.xed.view.html;

import io.github.greyp9.arwo.core.app.AppHtml;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xed.view.XedCursorView;
import io.github.greyp9.arwo.core.xed.view.XedPropertyPageView;
import io.github.greyp9.arwo.core.xed.view.XedTableView;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xpath.XPather;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.HttpURLConnection;

public class CursorHtmlView {
    private final ServletHttpRequest httpRequest;
    private final XedCursorView cursorView;

    public CursorHtmlView(final ServletHttpRequest httpRequest, final XedCursorView cursorView) {
        this.httpRequest = httpRequest;
        this.cursorView = cursorView;
    }

    public final HttpResponse doGetHtml() throws IOException {
        // template html
        final Document html = DocumentU.toDocument(StreamU.read(ResourceU.resolve(Const.HTML)));
        final Element body = new XPather(html, null).getElement(Html.XPath.BODY);
        // cursor content
        addContentTo(body);
        // touch ups
        new AppHtml(httpRequest).fixup(html);
        // package into response
        final byte[] entity = DocumentU.toXHtml(html);
        final NameTypeValue contentType = new NameTypeValue(Http.Header.CONTENT_TYPE, Http.Mime.TEXT_HTML_UTF8);
        final NameTypeValue contentLength = new NameTypeValue(Http.Header.CONTENT_LENGTH, entity.length);
        final NameTypeValues headers = new NameTypeValues(contentType, contentLength);
        return new HttpResponse(HttpURLConnection.HTTP_OK, headers, new ByteArrayInputStream(entity));
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    private void addContentTo(final Element html) throws IOException {
        final Object[] views = cursorView.getViews();
        for (final Object view : views) {
            if (view instanceof XedPropertyPageView) {
                final PropertyPageHtmlView htmlView = new PropertyPageHtmlView((XedPropertyPageView) view);
                htmlView.addContentTo(html);
            } else if (view instanceof XedTableView) {
                final TableHtmlView htmlView = new TableHtmlView((XedTableView) view);
                htmlView.addContentTo(html);
            }
        }
    }

    private static class Const {
        private static final String HTML = "io/github/greyp9/arwo/html/xed/xed.html";
    }
}
