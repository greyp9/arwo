package io.github.greyp9.arwo.core.xed.view.html;

import io.github.greyp9.arwo.core.alert.view.AlertsView;
import io.github.greyp9.arwo.core.app.AppHtml;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.menu.MenuSystem;
import io.github.greyp9.arwo.core.menu.view.MenuView;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.view.StatusBarView;
import io.github.greyp9.arwo.core.xed.action.XedActionCommit;
import io.github.greyp9.arwo.core.xed.action.XedActionLocale;
import io.github.greyp9.arwo.core.xed.menu.XedMenuFactory;
import io.github.greyp9.arwo.core.xed.request.XedRequest;
import io.github.greyp9.arwo.core.xed.state.XedUserState;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xpath.XPather;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.util.Locale;
import java.util.Properties;

@SuppressWarnings("PMD.AbstractNaming")
public abstract class HtmlView {
    private final XedRequest request;

    public final XedRequest getRequest() {
        return request;
    }

    public HtmlView(final XedRequest request) {
        this.request = request;
    }

    public final HttpResponse doGetHtml() throws IOException {
        final ServletHttpRequest httpRequest = request.getHttpRequest();
        final XedUserState userState = request.getState();
        // template html
        final Document html = DocumentU.toDocument(StreamU.read(ResourceU.resolve(Const.HTML)));
        final Element body = new XPather(html, null).getElement(Html.XPath.BODY);
        addMenuView(body);
        addTweaksView(body, userState);
        // cursor content
        addContentTo(body);
        // touch ups
        new AlertsView(request.getAlerts(), userState.getLocus()).addContentTo(body);
        new StatusBarView(httpRequest, userState.getLocus()).addContentTo(body);
        new AppHtml(httpRequest).fixup(html);
        // package into response
        final byte[] entity = DocumentU.toXHtml(html);
        final NameTypeValue contentType = new NameTypeValue(Http.Header.CONTENT_TYPE, Http.Mime.TEXT_HTML_UTF8);
        final NameTypeValue contentLength = new NameTypeValue(Http.Header.CONTENT_LENGTH, entity.length);
        final NameTypeValues headers = new NameTypeValues(contentType, contentLength);
        return new HttpResponse(HttpURLConnection.HTTP_OK, headers, new ByteArrayInputStream(entity));
    }

    private void addMenuView(final Element html) throws IOException {
        final Bundle bundle = request.getSession().getXed().getBundle();
        final ServletHttpRequest httpRequest = request.getHttpRequest();
        final MenuSystem menuSystem = request.getState().getMenuSystem();
        new MenuView(bundle, httpRequest, menuSystem).addContentTo(html, XedMenuFactory.Const.XED, true);
    }

    private void addTweaksView(final Element html, final XedUserState userState) throws IOException {
        // locale property strip
        final Locale locale = userState.getLocus().getLocale();
        final String submitID = userState.getSubmitID();
        final Properties properties = userState.getProperties();
        new XedActionLocale(locale).addContentTo(html, submitID, properties);
        new XedActionCommit(locale).addContentTo(html, submitID, properties);
    }

    public abstract void addContentTo(final Element html) throws IOException;

    private static class Const {
        private static final String HTML = "io/github/greyp9/arwo/html/xed/xed.html";
    }
}
