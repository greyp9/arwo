package io.github.greyp9.arwo.app.local.fs.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.favorite.FavoriteMenu;
import io.github.greyp9.arwo.app.core.view.props.AppPropertiesView;
import io.github.greyp9.arwo.app.core.view.refresh.AppRefreshView;
import io.github.greyp9.arwo.app.local.fs.core.LFSRequest;
import io.github.greyp9.arwo.core.alert.view.AlertsView;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppHtml;
import io.github.greyp9.arwo.core.app.AppTitle;
import io.github.greyp9.arwo.core.app.menu.AppMenuFactory;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.config.Preferences;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.html.upload.FileUpload;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.menu.view.MenuView;
import io.github.greyp9.arwo.core.text.filter.TextFilters;
import io.github.greyp9.arwo.core.util.PropertiesU;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.view.StatusBarView;
import io.github.greyp9.arwo.core.xed.action.XedActionLocale;
import io.github.greyp9.arwo.core.xed.action.XedActionRefresh;
import io.github.greyp9.arwo.core.xed.action.XedActionTextExpression;
import io.github.greyp9.arwo.core.xed.action.XedActionTextFilter;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xpath.XPather;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Locale;
import java.util.Properties;

@SuppressWarnings({ "PMD.AbstractNaming", "PMD.ExcessiveImports" })
public abstract class LFSView {
    private final LFSRequest request;
    private final File folderBase;
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final Bundle bundle;
    private final File file;

    public final LFSRequest getRequest() {
        return request;
    }

    public final AppUserState getUserState() {
        return userState;
    }

    public final File getFolderBase() {
        return folderBase;
    }

    public final Bundle getBundle() {
        return bundle;
    }

    public final File getFile() {
        return file;
    }

    @SuppressWarnings("WeakerAccess")
    public LFSView(final LFSRequest request, final AppUserState userState, final File folderBase, final File file) {
        this.request = request;
        this.folderBase = folderBase;
        this.httpRequest = request.getHttpRequest();
        this.userState = userState;
        this.bundle = request.getBundle();
        this.file = file;
    }

    public final HttpResponse doGetResponse() throws IOException {
        // template html
        final Document html = DocumentU.toDocument(StreamU.read(userState.getXHTML()));
        new AppRefreshView(userState.getProperties()).addContentTo(html.getDocumentElement());
        final Element body = new XPather(html, null).getElement(Html.XPath.BODY);
        // context-specific content
        final String modeKey = Value.join(Http.Token.DOT, App.Action.MENU, App.Mode.VIEW, request.getMode());
        final AppTitle title = AppTitle.Factory.getResourceLabel(
                httpRequest, bundle, request.getTitlePath(), userState.getCharset(), modeKey);
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
        final MenuView menuView = new MenuView(bundle, httpRequest, userState.getMenuSystem());
        final Element divMenus = menuView.addContentTo(html, AppMenuFactory.Const.FILESYSTEM, true);
        new FileUpload(httpRequest.getServletPath(), bundle).addContentTo(divMenus, menuView);
        // context title (with text filters)
        final Element divTitle = menuView.addTitle(html, title);
        addTextFiltersView(divTitle);
        // favorites (if toggled)
        final Xed xed = userState.getDocumentState().getSession(App.Servlet.FAVORITES).getXed();
        final Xed xedUI = userState.getXedFactory().getXedUI(xed, userState.getLocus().getLocale());
        final XedNav nav = new XedNav(xedUI);
        final XedCursor cursorFavorites = nav.findX("/app:favorites/app:lfsFavorites");  // i18n xpath
        final XedCursor cursorType = nav.find("lfsFavorite", cursorFavorites);  // i18n xpath
        // replacing table with menu bar
        new FavoriteMenu(getRequest(), userState, cursorType, AppMenuFactory.Const.FILESYSTEM).addContentTo(divMenus);
        //new AppFavoriteView(httpRequest, userState, cursorType, AppMenuFactory.Const.FILESYSTEM).addContentTo(html);
        // settings property strips
        final Locale locale = userState.getLocus().getLocale();
        final String submitID = userState.getSubmitID();
        final Properties properties = userState.getProperties();
        new XedActionLocale(userState.getXedFactory(), locale).addContentTo(html, submitID, properties);
        new XedActionRefresh(userState.getXedFactory(), locale).addContentTo(html, submitID, properties);
        final TextFilters textFilters = userState.getTextFilters(request.getContext());
        new XedActionTextExpression(userState.getXedFactory(), locale, textFilters).addContentTo(
                html, userState.getFiltersRecent(), submitID, properties);
        new XedActionTextFilter(userState.getXedFactory(), locale).addContentTo(html, submitID, properties);
    }

    private void addTextFiltersView(final Element html) {
        final TextFilters textFilters = userState.getTextFilters(getRequest().getContext());
        boolean isIncludes = !textFilters.getIncludes().isEmpty();
        boolean isExcludes = !textFilters.getExcludes().isEmpty();
        boolean isExpression = !textFilters.getExpressions().isEmpty();
        final Collection<String> tokens = new ArrayList<>();
        if (isExpression) {
            for (final String expression : textFilters.getExpressions()) {
                tokens.add("" + expression);
            }
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
        final Element divToolbar = ElementU.addElement(html, Html.DIV, null, NTV.create(Html.CLASS, App.CSS.MENU));
        for (final String token : tokens) {
            ElementU.addElement(divToolbar, Html.SPAN, token, NTV.create(Html.CLASS, App.CSS.MENU));
        }
    }

    protected final void addFileProperties(final Element html, final MetaFile metaFile) throws IOException {
        if (PropertiesU.isBoolean(userState.getProperties(), App.Action.PROPERTIES)) {
            final AppPropertiesView view = new AppPropertiesView("lfsPropertiesType", userState);  // i18n metadata
            view.addContentTo(html, metaFile, bundle, getFileProperties());
        }
    }

    private NameTypeValues getFileProperties() {
        return new NameTypeValues();
    }

    /**
     * Insert content into HTML page appropriate to the context of the subclass.
     *
     * @param html the wrapper HTML document
     * @return the http response containing the formatted content to be served to the requester
     * @throws IOException on failures accessing requested resources
     */
    protected abstract HttpResponse addContentTo(Element html) throws IOException;
}
