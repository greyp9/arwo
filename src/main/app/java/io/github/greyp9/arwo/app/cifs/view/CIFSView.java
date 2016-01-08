package io.github.greyp9.arwo.app.cifs.view;

import io.github.greyp9.arwo.app.cifs.connection.CIFSConnectionResource;
import io.github.greyp9.arwo.app.cifs.core.CIFSRequest;
import io.github.greyp9.arwo.app.cifs.data.CIFSDataSource;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.favorite.AppFavoriteView;
import io.github.greyp9.arwo.app.core.view.props.AppPropertiesView;
import io.github.greyp9.arwo.core.alert.view.AlertsView;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppHtml;
import io.github.greyp9.arwo.core.app.AppTitle;
import io.github.greyp9.arwo.core.app.menu.AppMenuFactory;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.html.upload.FileUpload;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.menu.view.MenuView;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.text.filter.TextFilters;
import io.github.greyp9.arwo.core.util.PropertiesU;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.view.StatusBarView;
import io.github.greyp9.arwo.core.xed.action.XedActionLocale;
import io.github.greyp9.arwo.core.xed.action.XedActionTextFilter;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
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

@SuppressWarnings({ "PMD.AbstractNaming", "PMD.ExcessiveImports" })
public abstract class CIFSView {
    private final CIFSRequest request;
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final Bundle bundle;
    private final CIFSConnectionResource resource;

    public final CIFSRequest getRequest() {
        return request;
    }

    public final AppUserState getUserState() {
        return userState;
    }

    public final CIFSConnectionResource getResource() {
        return resource;
    }

    public CIFSView(
            final CIFSRequest request, final AppUserState userState, final CIFSConnectionResource resource) {
        this.request = request;
        this.httpRequest = request.getHttpRequest();
        this.userState = userState;
        this.bundle = request.getBundle();
        this.resource = resource;
    }

    public final HttpResponse doGetResponse() throws IOException {
        // template html
        final Document html = DocumentU.toDocument(StreamU.read(ResourceU.resolve(App.Html.UI)));
        final Element body = new XPather(html, null).getElement(Html.XPath.BODY);
        // context-specific content
        final String modeKey = Value.join(Http.Token.DOT, App.Action.MENU, App.Mode.VIEW, request.getMode());
        final AppTitle title = AppTitle.Factory.getResourceLabel(
                httpRequest, bundle, request.getTitlePath(), userState.getCharset(), modeKey);
        addHeaderView(body, title);
        HttpResponse httpResponse = addContentTo(body);
        if (httpResponse == null) {
            // touch ups
            new AlertsView(userState.getAlerts(), userState.getLocus(), userState.getBundle(),
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
        final MenuView menuView = new MenuView(bundle, httpRequest, userState.getMenuSystem());
        final Element divMenus = menuView.addContentTo(html, AppMenuFactory.Const.FILESYSTEM, true);
        new FileUpload(httpRequest.getServletPath(), bundle).addContentTo(divMenus, menuView);
        // context title (with text filters)
        final Element divTitle = menuView.addTitle(html, title);
        addTextFiltersView(divTitle);
        // favorites (if toggled)
        final XedNav nav = new XedNav(userState.getDocumentState().getSession("/fav").getXed());
        final XedCursor cursorFavorites = nav.findX("/app:favorites/app:cifsFavorites");
        final XedCursor cursorType = nav.find("cifsFavorite", cursorFavorites);
        new AppFavoriteView(httpRequest, userState, cursorType, AppMenuFactory.Const.FILESYSTEM).addContentTo(html);
        // settings property strips
        final Locale locale = userState.getLocus().getLocale();
        final String submitID = userState.getSubmitID();
        final Properties properties = userState.getProperties();
        new XedActionLocale(locale).addContentTo(html, submitID, properties);
        new XedActionTextFilter(locale).addContentTo(html, submitID, properties);
        //new WebDAVConnectionView(httpRequest, userState, resource, bundle).addContent(html);
    }

    private void addTextFiltersView(final Element html) {
        final TextFilters textFilters = userState.getTextFilters();
        if (textFilters.isData()) {
            final Collection<String> tokens = new ArrayList<String>();
            // label
            tokens.add(bundle.getString("menu.view.textFilter"));
            // filter display
            final String patternInclude = bundle.getString("SFTPView.include");
            for (final String include : textFilters.getIncludes()) {
                tokens.add(MessageFormat.format(patternInclude, include));
            }
            final String patternExclude = bundle.getString("SFTPView.exclude");
            for (final String exclude : textFilters.getExcludes()) {
                tokens.add(MessageFormat.format(patternExclude, exclude));
            }
            // render
            final Element divToolbar = ElementU.addElement(html, Html.DIV, null, NTV.create(Html.CLASS, App.CSS.MENU));
            for (final String token : tokens) {
                ElementU.addElement(divToolbar, Html.SPAN, token, NTV.create(Html.CLASS, App.CSS.MENU));
            }
        }
    }

    protected final void addFileProperties(final Element html, final MetaFile metaFile) throws IOException {
        if (PropertiesU.isBoolean(userState.getProperties(), App.Action.PROPERTIES)) {
            final AppPropertiesView view = new AppPropertiesView("cifsPropertiesType", userState);
            view.addContentTo(html, metaFile, bundle, getFileProperties());
        }
    }

    private NameTypeValues getFileProperties() throws IOException {
        final CIFSDataSource source = new CIFSDataSource(request, resource.getConnection());
        return source.properties(request.getPath());
    }

    protected abstract HttpResponse addContentTo(Element html) throws IOException;
}
