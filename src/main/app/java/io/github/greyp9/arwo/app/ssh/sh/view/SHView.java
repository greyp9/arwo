package io.github.greyp9.arwo.app.ssh.sh.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.favorite.AppFavoriteView;
import io.github.greyp9.arwo.app.ssh.connection.SSHConnectionResource;
import io.github.greyp9.arwo.app.ssh.core.view.SSHConnectionView;
import io.github.greyp9.arwo.app.ssh.sh.core.SHRequest;
import io.github.greyp9.arwo.core.alert.view.AlertsView;
import io.github.greyp9.arwo.core.app.App;
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
import io.github.greyp9.arwo.core.xed.action.XedActionLocale;
import io.github.greyp9.arwo.core.xed.action.XedActionTextFilter;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xpath.XPather;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.util.Locale;
import java.util.Properties;

@SuppressWarnings({ "PMD.AbstractNaming", "PMD.ExcessiveImports" })
public abstract class SHView {
    private final SHRequest request;
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final Bundle bundle;

    public final SHRequest getRequest() {
        return request;
    }

    public final AppUserState getUserState() {
        return userState;
    }

    public SHView(final SHRequest request, final AppUserState userState) {
        this.request = request;
        this.httpRequest = request.getHttpRequest();
        this.userState = userState;
        this.bundle = request.getBundle();
    }

    public final HttpResponse doGetResponse() throws IOException {
        // template html
        final Document html = DocumentU.toDocument(StreamU.read(ResourceU.resolve(App.Html.UI)));
        final Element body = new XPather(html, null).getElement(Html.XPath.BODY);
        // context-specific content
        final AppTitle title = AppTitle.Factory.getResourceLabel(httpRequest, bundle, request.getTitlePath());
        addHeaderView(body, title);
        HttpResponse httpResponse = addContentTo(body);
        if (httpResponse == null) {
            // touch ups
            final boolean displayAlerts = (httpRequest.getHttpRequest().getHeader(App.Header.RESULT) == null);
            new AlertsView(displayAlerts, userState.getAlerts(), userState.getLocus(), userState.getBundle(),
                    userState.getSubmitID()).addContentTo(body);
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

    private void addHeaderView(final Element html, final AppTitle title) throws IOException {
        // context menu
        final MenuView menuView = new MenuView(request.getBundle(), httpRequest, userState.getMenuSystem());
        menuView.addContentTo(html, AppMenuFactory.Const.COMMAND, true);
        // context title
        menuView.addTitle(html, title);
        // favorites (if toggled)
        final Xed xed = userState.getDocumentState().getSession(App.Servlet.FAVORITES).getXed();
        final Xed xedX = userState.getXedFactory().getXedUI(xed, userState.getLocale());
        final XedNav nav = new XedNav(xedX);
        final XedCursor cursorFavorites = nav.findX("/app:favorites/app:sshFavorites");  // i18n xpath
        final XedCursor cursorType = nav.find("sshFavorite", cursorFavorites);  // i18n xpath
        new AppFavoriteView(httpRequest, userState, cursorType, AppMenuFactory.Const.COMMAND).addContentTo(html);
        // settings property strips
        final Locale locale = userState.getLocus().getLocale();
        final String submitID = userState.getSubmitID();
        final Properties properties = userState.getProperties();
        new XedActionLocale(userState.getXedFactory(), locale).addContentTo(html, submitID, properties);
        new XedActionTextFilter(userState.getXedFactory(), locale).addContentTo(html, submitID, properties);
        final SSHConnectionResource resource = (SSHConnectionResource)
                userState.getSSH().getCache().getResource(request.getServer(), null);
        new SSHConnectionView(httpRequest, userState, resource, bundle).addContent(html);
    }

    protected abstract HttpResponse addContentTo(Element html) throws IOException;
}
