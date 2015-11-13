package io.github.greyp9.arwo.core.xed.view.html;

import io.github.greyp9.arwo.core.app.AppHtml;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.view.StatusBarView;
import io.github.greyp9.arwo.core.xed.action.XedActionLocale;
import io.github.greyp9.arwo.core.xed.request.XedRequest;
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
import java.util.Locale;

public class CursorHtmlView {
    private final XedCursorView cursorView;
    private final XedRequest request;

    public CursorHtmlView(final XedCursorView cursorView, final XedRequest request) {
        this.cursorView = cursorView;
        this.request = request;
    }

    public final HttpResponse doGetHtml() throws IOException {
        // template html
        final Document html = DocumentU.toDocument(StreamU.read(ResourceU.resolve(Const.HTML)));
        final Element body = new XPather(html, null).getElement(Html.XPath.BODY);
        // locale property strip
        final Locale locale = request.getState().getLocus().getLocale();
        new XedActionLocale(locale).addContentTo(body, locale.getLanguage(), request.getState().getSubmitID());
        // cursor content
        addContentTo(body);
        // touch ups
        new AppHtml(request.getHttpRequest()).fixup(html);
        new StatusBarView(request.getHttpRequest(), request.getState().getLocus()).addContentTo(body);
        // package into response
        final byte[] entity = DocumentU.toXHtml(html);
        final NameTypeValue contentType = new NameTypeValue(Http.Header.CONTENT_TYPE, Http.Mime.TEXT_HTML_UTF8);
        final NameTypeValue contentLength = new NameTypeValue(Http.Header.CONTENT_LENGTH, entity.length);
        final NameTypeValues headers = new NameTypeValues(contentType, contentLength);
        return new HttpResponse(HttpURLConnection.HTTP_OK, headers, new ByteArrayInputStream(entity));
    }

    private void addContentTo(final Element html) throws IOException {
        new BreadcrumbsHtmlView(cursorView.getBaseURI(), cursorView.getCursor()).addContentTo(html);
        final Object[] views = cursorView.getViews();
        for (final Object view : views) {
            if (view instanceof XedPropertyPageView) {
                addPropertyPage(html, (XedPropertyPageView) view);
            } else if (view instanceof XedTableView) {
                addTable(html, (XedTableView) view);
            }
        }
    }

    private void addPropertyPage(final Element html, final XedPropertyPageView view) throws IOException {
        final PropertyPageHtmlView htmlView = new PropertyPageHtmlView(view, request);
        htmlView.addContentTo(html);
    }

    private void addTable(final Element html, final XedTableView view) throws IOException {
        final TableHtmlView htmlView = new TableHtmlView(view, request);
        htmlView.addContentTo(html);
    }

    private static class Const {
        private static final String HTML = "io/github/greyp9/arwo/html/xed/xed.html";
    }
}
