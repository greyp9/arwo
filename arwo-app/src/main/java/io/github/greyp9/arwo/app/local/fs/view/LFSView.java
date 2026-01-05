package io.github.greyp9.arwo.app.local.fs.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.fixup.AppHtmlView;
import io.github.greyp9.arwo.app.core.view.props.AppPropertiesView;
import io.github.greyp9.arwo.app.core.view.rename.AppFilesRenameView;
import io.github.greyp9.arwo.app.local.fs.core.LFSRequest;
import io.github.greyp9.arwo.app.local.fs.menu.MenuFavLFS;
import io.github.greyp9.arwo.app.local.fs.menu.MenuLFS;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppTitle;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.menu2.model.MenuItem;
import io.github.greyp9.arwo.core.menu2.view.MenuHtml;
import io.github.greyp9.arwo.core.util.PropertiesU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xpath.XPather;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.Optional;

@SuppressWarnings({ "PMD.AbstractNaming", "PMD.ExcessiveImports" })
public abstract class LFSView {
    private final LFSRequest request;
    private final File folderBase;
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final Bundle bundle;
    private final File file;
    private final AppTitle title;

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

        final String modeKey = Value.join(Http.Token.DOT, App.Action.MENU, App.Mode.VIEW, request.getMode());
        final boolean cacheItem = PropertiesU.isBoolean(userState.getProperties(), App.Action.CACHE);
        this.title = AppTitle.Factory.getResourceLabel(httpRequest, bundle,
                request.getTitlePath(), userState.getCharset(), modeKey, String.format("cache=%s", cacheItem));
    }

    public final HttpResponse doGetResponse() throws IOException {
        final Document html = DocumentU.toDocument(StreamU.read(userState.getXHTML()));
        final Element header = new XPather(html, null).getElement(Html.XPath.HEADER);
        final Element content = new XPather(html, null).getElement(Html.XPath.CONTENT);
        final Element footer = new XPather(html, null).getElement(Html.XPath.FOOTER);
        final HttpResponse httpResponse = addContentTo(content);
        return Optional.ofNullable(httpResponse).orElse(
                new AppHtmlView(httpRequest, userState, title)
                        .appRefreshView(html)
                        .uploadFile(html, userState.getMenuSystemState().getProperty(MENU_KEY_FILE_UPLOAD))
                        .title(header)
                        .addTextFiltersView(header)
                        .propertyStrips(header)
                        .actionRefresh(header)
                        .actionTextExpression(header)
                        .alerts(header)
                        .statusBar(footer)
                        .appHtml(html)
                        .toHttpResponse(html));
    }

    protected final void addMenusLFS(final Element header) throws IOException {
        final MenuItem menuFavorites = new MenuFavLFS(request.getBaseURIMode(), userState).toMenuItem()
                .applyFrom(userState.getMenuSystemState());
        final MenuItem menu = new MenuLFS().toMenuItem()
                .applyFrom(userState.getMenuSystemState());
        final MenuHtml menuHtml = new MenuHtml(httpRequest, userState.getBundle(), userState.getSubmitID(), STYLE_HOME);
        menuHtml.addTo(header, false, "v", Collections.singletonList(menuFavorites));
        menuHtml.addTo(header, true, "m", Collections.singletonList(menu));
    }

    private static final String MENU_KEY_FILE_UPLOAD = "/menu2/lfs/file/upload";
    private static final String STYLE_HOME = "background-color: brown; color: white;";

    protected final void addFileProperties(final Element html, final MetaFile metaFile) throws IOException {
        if (PropertiesU.isBoolean(userState.getProperties(), App.Action.PROPERTIES)) {
            final AppPropertiesView view = new AppPropertiesView("lfsPropertiesType", userState);  // i18n metadata
            view.addContentTo(html, metaFile, bundle, getFileProperties());
        }
    }

    protected final void addFilesRename(final Element html) throws IOException {
        if (PropertiesU.isBoolean(userState.getProperties(), App.Mode.RENAME_F)) {
            final AppFilesRenameView view = new AppFilesRenameView(request.getHttpRequest(), userState);  // i18n meta
            view.addContentTo(html);
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
