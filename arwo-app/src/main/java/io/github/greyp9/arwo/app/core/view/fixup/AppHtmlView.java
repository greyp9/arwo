package io.github.greyp9.arwo.app.core.view.fixup;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.refresh.AppRefreshView;
import io.github.greyp9.arwo.core.alert.view.AlertsView;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppHtml;
import io.github.greyp9.arwo.core.app.AppTitle;
import io.github.greyp9.arwo.core.config.Preferences;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.menu.MenuContext;
import io.github.greyp9.arwo.core.menu.view.MenuView;
import io.github.greyp9.arwo.core.text.filter.TextFilters;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.view.StatusBarView;
import io.github.greyp9.arwo.core.xed.action.XedActionLocale;
import io.github.greyp9.arwo.core.xed.action.XedActionRefresh;
import io.github.greyp9.arwo.core.xed.action.XedActionTextExpression;
import io.github.greyp9.arwo.core.xed.action.XedActionTextFilter;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xpath.XPather;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.util.Locale;
import java.util.Properties;

public class AppHtmlView {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final AppTitle title;
    private final MenuContext menuContext;
    private final String augments;

    public AppHtmlView(final ServletHttpRequest httpRequest,
                       final AppUserState userState,
                       final AppTitle title,
                       final MenuContext menuContext,
                       final String augments) {
        this.httpRequest = httpRequest;
        this.userState = userState;
        this.title = title;
        this.menuContext = menuContext;
        this.augments = augments;
    }

    public final HttpResponse fixup(final Document html) throws IOException {
        new AppRefreshView(userState.getProperties()).addContentTo(html.getDocumentElement());  // meta refresh
        // body
        final Element header = new XPather(html, null).getElement(Html.XPath.HEADER);
        final Element footer = new XPather(html, null).getElement(Html.XPath.FOOTER);
        // menus; title
        final MenuView menuView = new MenuView(userState.getBundle(), httpRequest, menuContext);  // render
        menuView.addMenusTo(header);
        menuView.addTitle(header, title);
        // settings property strips
        final Locale locale = userState.getLocus().getLocale();
        final String submitID = userState.getSubmitID();
        final Properties properties = userState.getProperties();
        if (augments.contains(LOCALE)) {
            new XedActionLocale(userState.getXedFactory(), locale).addContentTo(header, submitID, properties);
        }
        new XedActionRefresh(userState.getXedFactory(), locale).addContentTo(header, submitID, properties);
        if (augments.contains(EXPRESSION)) {
            final TextFilters textFilters = userState.getTextFilters("");
            new XedActionTextExpression(userState.getXedFactory(), locale, textFilters).addContentTo(
                    header, userState.getFiltersRecent(), submitID, properties);
            new XedActionTextFilter(userState.getXedFactory(), locale).addContentTo(header, submitID, properties);
        }
        // alerts
        final boolean displayAlerts = (httpRequest.getHttpRequest().getHeader(App.Header.RESULT) == null);
        new AlertsView(displayAlerts, userState.getAlerts(), userState.getLocus(), userState.getBundle(),
                userState.getSubmitID()).addContentTo(header);
        // status bar
        new StatusBarView(httpRequest, userState.getLocus()).addContentTo(footer);
        // UI preferences
        final Preferences preferences = new Preferences(userState.getConfig());
        final String iconColor = Value.defaultOnEmpty(preferences.getIconColor(), "black");
        final String theme = Value.defaultOnEmpty(preferences.getTheme(), "default");
        new AppHtml(httpRequest).fixup(html, title, iconColor, theme);
        // package into response
        final byte[] entity = DocumentU.toXHtml(html);
        final NameTypeValue contentType = new NameTypeValue(Http.Header.CONTENT_TYPE, Http.Mime.TEXT_HTML_UTF8);
        final NameTypeValue contentLength = new NameTypeValue(Http.Header.CONTENT_LENGTH, entity.length);
        final NameTypeValues headers = new NameTypeValues(contentType, contentLength);
        return new HttpResponse(HttpURLConnection.HTTP_OK, headers, new ByteArrayInputStream(entity));
    }

    public static final String LOCALE = "locale";
    public static final String EXPRESSION = "expression";
}
