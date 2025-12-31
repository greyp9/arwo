package io.github.greyp9.arwo.app.core.view.fixup;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.refresh.AppRefreshView;
import io.github.greyp9.arwo.core.alert.view.AlertsView;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppHtml;
import io.github.greyp9.arwo.core.app.AppTitle;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.config.Preferences;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.html.upload.FileUpload;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.menu.MenuContext;
import io.github.greyp9.arwo.core.menu.view.MenuView;
import io.github.greyp9.arwo.core.text.filter.TextFilters;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.view.StatusBarView;
import io.github.greyp9.arwo.core.xed.action.XedActionLocale;
import io.github.greyp9.arwo.core.xed.action.XedActionRefresh;
import io.github.greyp9.arwo.core.xed.action.XedActionTextExpression;
import io.github.greyp9.arwo.core.xed.action.XedActionTextFilter;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xpath.XPather;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
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

    public final AppHtmlView appRefreshView(final Document html) throws IOException {
        new AppRefreshView(userState.getProperties()).addContentTo(html.getDocumentElement());  // meta refresh
        return this;
    }

    public final AppHtmlView uploadFile(final Document html, final String isFileUpload) throws IOException {
        final Element divMenuFile = new XPather(html, null).getElement(
                "/html/body//div[button/text()[contains(.,'Upload')]]");
        if (divMenuFile != null) {
            new FileUpload(httpRequest.getServletPath(), userState.getBundle())
                    .addContentTo((Element) divMenuFile.getParentNode(), isFileUpload);
        }
        return this;
    }

    public final AppHtmlView title(final Element html) throws IOException {
        final Element divMenus = ElementU.addElement(html, Html.DIV, null, NTV.create(Html.CLASS, App.CSS.MENUS));
        final Element div = ElementU.addElement(divMenus, Html.DIV, null, NTV.create(Html.CLASS, App.CSS.PAGE_TITLE));
        ElementU.addElement(div, Html.IMG, null,
                NTV.create(Html.SRC, "${CONTEXT}/ico/app-black.ico", Html.ALT, ""));
        ElementU.addElement(div, Html.SPAN, title.getText());
        return this;
    }

    public final AppHtmlView propertyStrips(final Element header) throws IOException {
        final Locale locale = userState.getLocus().getLocale();
        final String submitID = userState.getSubmitID();
        final Properties properties = userState.getProperties();
        new XedActionLocale(userState.getXedFactory(), locale).addContentTo(header, submitID, properties);
        return this;
    }

    public final AppHtmlView actionRefresh(final Element header) throws IOException {
        final Locale locale = userState.getLocus().getLocale();
        final String submitID = userState.getSubmitID();
        final Properties properties = userState.getProperties();
        new XedActionRefresh(userState.getXedFactory(), locale).addContentTo(header, submitID, properties);
        return this;
    }

    public final AppHtmlView actionLocale(final Element header) throws IOException {
        final Locale locale = userState.getLocus().getLocale();
        final String submitID = userState.getSubmitID();
        final Properties properties = userState.getProperties();
        new XedActionLocale(userState.getXedFactory(), locale).addContentTo(header, submitID, properties);
        return this;
    }

    public final AppHtmlView actionTextExpression(final Element header) throws IOException {
        final Locale locale = userState.getLocus().getLocale();
        final String submitID = userState.getSubmitID();
        final Properties properties = userState.getProperties();
        final TextFilters textFilters = userState.getTextFilters("");
        new XedActionTextExpression(userState.getXedFactory(), locale, textFilters).addContentTo(
                header, userState.getFiltersRecent(), submitID, properties);
        new XedActionTextFilter(userState.getXedFactory(), locale).addContentTo(header, submitID, properties);
        return this;
    }

    public final AppHtmlView alerts(final Element header) throws IOException {
        final boolean displayAlerts = (httpRequest.getHttpRequest().getHeader(App.Header.RESULT) == null);
        new AlertsView(displayAlerts, userState.getAlerts(), userState.getLocus(), userState.getBundle(),
                userState.getSubmitID()).addContentTo(header);
        return this;
    }

    public final AppHtmlView statusBar(final Element footer) throws IOException {
        new StatusBarView(httpRequest, userState.getLocus()).addContentTo(footer);
        return this;
    }

    public final AppHtmlView appHtml(final Document html) throws IOException {
        final Preferences preferences = new Preferences(userState.getConfig());
        final String iconColor = Value.defaultOnEmpty(preferences.getIconColor(), "black");
        final String theme = Value.defaultOnEmpty(preferences.getTheme(), "default");
        new AppHtml(httpRequest).fixup(html, title, iconColor, theme);
        return this;
    }

    public final HttpResponse toHttpResponse(final Document html) throws IOException {
        final byte[] entity = DocumentU.toXHtml(html);
        final NameTypeValue contentType = new NameTypeValue(Http.Header.CONTENT_TYPE, Http.Mime.TEXT_HTML_UTF8);
        final NameTypeValue contentLength = new NameTypeValue(Http.Header.CONTENT_LENGTH, entity.length);
        final NameTypeValues headers = new NameTypeValues(contentType, contentLength);
        return new HttpResponse(HttpURLConnection.HTTP_OK, headers, new ByteArrayInputStream(entity));
    }

    public final HttpResponse fixup(final Document html) throws IOException {
        new AppRefreshView(userState.getProperties()).addContentTo(html.getDocumentElement());  // meta refresh
        // body
        final Element header = new XPather(html, null).getElement(Html.XPath.HEADER);
        final Element footer = new XPather(html, null).getElement(Html.XPath.FOOTER);
        // menus; title
        final MenuView menuView = new MenuView(userState.getBundle(), httpRequest, menuContext);  // render
        menuView.addMenusTo(header);
        // upload menu
        final Element divMenuFile = new XPather(html, null).getElement(
                "/html/body//div[button/text()[contains(.,'Upload')]]");
        if (divMenuFile != null) {
            new FileUpload(httpRequest.getServletPath(), userState.getBundle())
                    .addContentTo((Element) divMenuFile.getParentNode(), menuView);
        }
        menuView.addTitle(header, title);
        addTextFiltersView(header);
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

    public final AppHtmlView addTextFiltersView(final Element html) {
        final Bundle bundle = userState.getBundle();
        final TextFilters textFilters = userState.getTextFilters("");
        boolean isIncludes = !textFilters.getIncludes().isEmpty();
        boolean isExcludes = !textFilters.getExcludes().isEmpty();
        boolean isExpression = !textFilters.getExpressions().isEmpty();
        final Collection<String> tokens = new ArrayList<>();
        if (isExpression) {
            tokens.addAll(textFilters.getExpressions());
        } else if (isIncludes || isExcludes) {
            // label
            tokens.add(bundle.getString("menu.view.textFilter"));
            final String patternInclude = bundle.getString("SFTPView.include");
            for (final String include : textFilters.getIncludes()) {
                tokens.add(MessageFormat.format(patternInclude, include));
            }
            final String patternExclude = bundle.getString("SFTPView.exclude");
            for (final String exclude : textFilters.getExcludes()) {
                tokens.add(MessageFormat.format(patternExclude, exclude));
            }
        }
        // render
        if (!tokens.isEmpty()) {
            final Element divToolbar = ElementU.addElement(html, Html.DIV, null, NTV.create(Html.CLASS, App.CSS.MENU));
            for (final String token : tokens) {
                ElementU.addElement(divToolbar, Html.SPAN, token, NTV.create(Html.CLASS, App.CSS.MENU));
            }
        }
        return this;
    }

    public static final String LOCALE = "locale";
    public static final String EXPRESSION = "expression";
    public static final String UPLOAD = "upload";
}
