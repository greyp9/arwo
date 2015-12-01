package io.github.greyp9.arwo.app.ssh.core.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.alert.view.AlertsView;
import io.github.greyp9.arwo.core.app.AppHtml;
import io.github.greyp9.arwo.core.app.AppText;
import io.github.greyp9.arwo.core.app.AppTitle;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.view.StatusBarView;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xpath.XPather;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.HttpURLConnection;

public class SSHView {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;

    public SSHView(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.httpRequest = httpRequest;
        this.userState = userState;
    }

    public final HttpResponse doGetHtmlInventory(final String offsetURI) throws IOException {
        // template html
        final Document html = DocumentU.toDocument(StreamU.read(ResourceU.resolve(Const.HTML)));
        final Element body = new XPather(html, null).getElement(Html.XPath.BODY);
        //addMenuView(body);
        //addTweaksView(body, userState);
        // cursor content
        new SSHInventoryView(httpRequest, userState, offsetURI).addContent(body);
        // touch ups
        new AlertsView(userState.getAlerts(), userState.getLocus()).addContentTo(body);
        new StatusBarView(httpRequest, userState.getLocus()).addContentTo(body);
        final Locus locus = userState.getLocus();
        final Bundle bundle = new Bundle(new AppText(locus.getLocale()).getBundleCore());
        final AppTitle title = AppTitle.Factory.getHostLabel(httpRequest, bundle);
        new AppHtml(httpRequest).fixup(html, title);
        // package into response
        final byte[] entity = DocumentU.toXHtml(html);
        final NameTypeValue contentType = new NameTypeValue(Http.Header.CONTENT_TYPE, Http.Mime.TEXT_HTML_UTF8);
        final NameTypeValue contentLength = new NameTypeValue(Http.Header.CONTENT_LENGTH, entity.length);
        final NameTypeValues headers = new NameTypeValues(contentType, contentLength);
        return new HttpResponse(HttpURLConnection.HTTP_OK, headers, new ByteArrayInputStream(entity));
    }

    private static class Const {
        private static final String HTML = "io/github/greyp9/arwo/html/xed/xed.html";
    }
}
