package io.github.greyp9.arwo.app.local.sh.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.favorite.AppFavoriteView;
import io.github.greyp9.arwo.app.core.view.refresh.AppRefreshView;
import io.github.greyp9.arwo.app.local.sh.core.SHRequest;
import io.github.greyp9.arwo.core.action.ActionItem;
import io.github.greyp9.arwo.core.alert.view.AlertsView;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppHtml;
import io.github.greyp9.arwo.core.app.AppTitle;
import io.github.greyp9.arwo.core.app.menu.AppMenuFactory;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.config.Preferences;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.html.menu.ActionItemsMenu;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.io.script.Script;
import io.github.greyp9.arwo.core.menu.view.MenuView;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
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
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.Properties;
import java.util.stream.Collectors;

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
        final Document html = DocumentU.toDocument(StreamU.read(userState.getXHTML()));
        new AppRefreshView(userState.getProperties()).addContentTo(html.getDocumentElement());
        final Element body = new XPather(html, null).getElement(Html.XPath.BODY);
        // context-specific content
        final AppTitle title = AppTitle.Factory.getHostLabel(httpRequest, bundle);
        addHeaderView(body, title);
        HttpResponse httpResponse = addContentTo(body);
        if (httpResponse == null) {
            // touch ups
            final boolean displayAlerts = (httpRequest.getHttpRequest().getHeader(App.Header.RESULT) == null);
            new AlertsView(displayAlerts, userState.getAlerts(), userState.getLocus(), userState.getBundle(),
                    userState.getSubmitID()).addContentTo(body);
            new StatusBarView(httpRequest, userState.getLocus()).addContentTo(body);
            final Preferences preferences = new Preferences(getUserState().getConfig());
            final String iconColor = Value.defaultOnEmpty(preferences.getIconColor(), "black");
            final String theme = Value.defaultOnEmpty(preferences.getTheme(), "default");
            new AppHtml(httpRequest).fixup(html, title, iconColor, theme);
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
        final Element divMenus = menuView.addContentTo(html, AppMenuFactory.Const.COMMAND, true);
        final Collection<Script> scripts = userState.getLocal().getHistory().getHistory();
        final List<ActionItem> actionItems = scripts.stream()
                .map(Script::getContext)
                .distinct()
                .sorted()
                .map(c -> new ActionItem(c, null, null, null, null))
                .collect(Collectors.toList());
        new ActionItemsMenu("[Contexts]", httpRequest.getBaseURI(), actionItems).addContentTo(divMenus);
        // context title
        menuView.addTitle(html, title);
        // favorites (if toggled)
        final Xed xed = userState.getDocumentState().getSession(App.Servlet.FAVORITES).getXed();
        final Xed xedX = userState.getXedFactory().getXedUI(xed, userState.getLocale());
        final XedNav nav = new XedNav(xedX);
        final XedCursor cursorFavorites = nav.findX("/app:favorites/app:lshFavorites");  // i18n xpath
        final XedCursor cursorType = nav.find("lshFavorite", cursorFavorites);  // i18n xpath
        new AppFavoriteView(httpRequest, userState, cursorType, AppMenuFactory.Const.COMMAND).addContentTo(html);
        // settings property strips
        final Locale locale = userState.getLocus().getLocale();
        final String submitID = userState.getSubmitID();
        final Properties properties = userState.getProperties();
        new XedActionLocale(userState.getXedFactory(), locale).addContentTo(html, submitID, properties);
        new XedActionTextFilter(userState.getXedFactory(), locale).addContentTo(html, submitID, properties);
    }

    protected abstract HttpResponse addContentTo(Element html) throws IOException;
}
