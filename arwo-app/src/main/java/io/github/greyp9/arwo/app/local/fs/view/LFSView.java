package io.github.greyp9.arwo.app.local.fs.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.favorite.FavoriteMenu;
import io.github.greyp9.arwo.app.core.view.fixup.AppHtmlView;
import io.github.greyp9.arwo.app.core.view.props.AppPropertiesView;
import io.github.greyp9.arwo.app.core.view.rename.AppFilesRenameView;
import io.github.greyp9.arwo.app.local.fs.core.LFSRequest;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.AppTitle;
import io.github.greyp9.arwo.core.app.menu.AppMenuFactory;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.menu.MenuContext;
import io.github.greyp9.arwo.core.menu.MenuItem;
import io.github.greyp9.arwo.core.menu.MenuSystem;
import io.github.greyp9.arwo.core.menu.view.MenuView;
import io.github.greyp9.arwo.core.util.PropertiesU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
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
    private final MenuContext menuContext;

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
        this.title = AppTitle.Factory.getResourceLabel(
                httpRequest, bundle, request.getTitlePath(), userState.getCharset(), modeKey);
        final MenuSystem menuSystem = userState.getMenuSystem();
        final MenuItem menuItem = menuSystem.get(httpRequest.getServletPath(), AppMenuFactory.Const.FILESYSTEM);
        this.menuContext = new MenuContext(menuSystem, Collections.singletonList(menuItem));
    }

    public final HttpResponse doGetResponse() throws IOException {
        final Document html = DocumentU.toDocument(StreamU.read(userState.getXHTML()));
        final Element body = new XPather(html, null).getElement(Html.XPath.CONTENT);
        final HttpResponse httpResponse = addContentTo(body);
        return Optional.ofNullable(httpResponse)
                .orElse(new AppHtmlView(httpRequest, userState, title, menuContext, "").fixup(html));
    }

    protected final void addMenusLFS(final Element html) throws IOException {
        // context menu
        final MenuView menuView = new MenuView(bundle, httpRequest, userState.getMenuSystem());
        final Element divMenusSticky = menuView.addContentTo(
                html, AppMenuFactory.Const.FILESYSTEM_STICKY, null, false, true, "v");
        // favorites (if toggled)
        final Xed xed = userState.getDocumentState().getSession(App.Servlet.FAVORITES).getXed();
        final Xed xedUI = userState.getXedFactory().getXedUI(xed, userState.getLocus().getLocale());
        final XedNav nav = new XedNav(xedUI);
        final XedCursor cursorFavorites = nav.findX("/app:favorites/app:lfsFavorites");  // i18n xpath
        final XedCursor cursorType = nav.find("lfsFavorite", cursorFavorites);  // i18n xpath
        // replacing table with menu bar
        new FavoriteMenu(getRequest(), userState, cursorType, AppMenuFactory.Const.FILESYSTEM_STICKY)
                .addContentTo(divMenusSticky);
    }

    protected final void addFileProperties(final Element html, final MetaFile metaFile) throws IOException {
        if (PropertiesU.isBoolean(userState.getProperties(), App.Action.PROPERTIES)) {
            final AppPropertiesView view = new AppPropertiesView("lfsPropertiesType", userState);  // i18n metadata
            view.addContentTo(html, metaFile, bundle, getFileProperties());
        }
    }

    protected final void addFilesRename(final Element html) throws IOException {
        if (PropertiesU.isBoolean(userState.getProperties(), App.Action.FILES_RENAME)) {
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
