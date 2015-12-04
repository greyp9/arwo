package io.github.greyp9.arwo.app.ssh.sftp.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.ssh.connection.SSHConnectionResource;
import io.github.greyp9.arwo.app.ssh.sftp.core.SFTPRequest;
import io.github.greyp9.arwo.core.alert.view.AlertsView;
import io.github.greyp9.arwo.core.app.AppHtml;
import io.github.greyp9.arwo.core.app.AppTitle;
import io.github.greyp9.arwo.core.app.menu.AppMenuFactory;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.menu.view.MenuView;
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

@SuppressWarnings("PMD.AbstractNaming")
public abstract class SFTPView {
    private final SFTPRequest request;
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final Bundle bundle;
    private final SSHConnectionResource resource;

    public final SFTPRequest getRequest() {
        return request;
    }

    public final AppUserState getUserState() {
        return userState;
    }

    public final SSHConnectionResource getResource() {
        return resource;
    }

    public SFTPView(final SFTPRequest request, final AppUserState userState, final SSHConnectionResource resource) {
        this.request = request;
        this.httpRequest = request.getHttpRequest();
        this.userState = userState;
        this.bundle = request.getBundle();
        this.resource = resource;
    }

    public final HttpResponse doGetResponse() throws IOException {
        // template html
        final Document html = DocumentU.toDocument(StreamU.read(ResourceU.resolve(Const.HTML)));
        final Element body = new XPather(html, null).getElement(Html.XPath.BODY);
        // context-specific content
        final AppTitle title = AppTitle.Factory.getResourceLabel(httpRequest, bundle, request.getTitlePath());
        final MenuView menuView = new MenuView(request.getBundle(), httpRequest, userState.getMenuSystem());
        menuView.addContentTo(body, AppMenuFactory.Const.FILESYSTEM, true);
        menuView.addTitle(body, title);
        //if (userState.isToggleProperties()) {
        //    new AppPropertiesView(PROPERTIES_TABLE_ID, userState).addContent(body, null, getFileProperties());
        //}
        HttpResponse httpResponse = addContentTo(body);
        //new TextFilterView(userState.getLocus().getLocale(), userState.getSubmitID()).
        //        addUI(userState.isToggleTextFilterForm(), body);
        if (httpResponse == null) {
            // touch ups
            new AlertsView(userState.getAlerts(), userState.getLocus()).addContentTo(body);
            new StatusBarView(httpRequest, userState.getLocus()).addContentTo(body);
            new AppHtml(httpRequest).fixup(html, title);
            // package into response
            final byte[] entity = DocumentU.toXHtml(html);
            final NameTypeValue contentType = new NameTypeValue(Http.Header.CONTENT_TYPE, Http.Mime.TEXT_HTML_UTF8);
            final NameTypeValue contentLength = new NameTypeValue(Http.Header.CONTENT_LENGTH, entity.length);
            final NameTypeValues headers = new NameTypeValues(contentType, contentLength);
            httpResponse = new HttpResponse(HttpURLConnection.HTTP_OK, headers, new ByteArrayInputStream(entity));
        }
        return httpResponse;
    }

    protected abstract HttpResponse addContentTo(Element html) throws IOException;

    private static class Const {
        private static final String HTML = "io/github/greyp9/arwo/html/xed/xed.html";
    }
}
