package io.github.greyp9.arwo.app.core.view.hex;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.hex.HexRenderer;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.lang.NumberU;
import io.github.greyp9.arwo.core.menu2.model.MenuItem;
import io.github.greyp9.arwo.core.menu2.view.MenuHtml;
import io.github.greyp9.arwo.core.page.Page;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Collections;

public class AppHexView {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;

    public AppHexView(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.httpRequest = httpRequest;
        this.userState = userState;
    }

    public final HttpResponse addContentTo(
            final Element html, final MetaFile metaFile, final Bundle bundle) throws IOException {
        httpRequest.getClass();
        // setup
        final byte[] bytes = StreamU.read(metaFile.getBytes());
        userState.setPageViewHex(Page.Factory.fixPage(userState.getPageViewHex(), bytes.length));
        final Page page = userState.getPageViewHex();
        final int start = page.getPosition();
        final int finish = Math.min(start + page.getCount(), bytes.length);
        // context menu
/*
        final String position = bundle.format("table.page.n.to.m.of.x", start, finish, bytes.length);
        final MenuView menuView = new MenuView(bundle, httpRequest, userState.getMenuSystem());
        menuView.addContentTo(html, AppMenuFactory.Const.HEX, null, false, false, Html.VALUE_6, position);
*/
        final MenuItem menu = new MenuHex().toMenuItem();
        final MenuHtml menuHtml = new MenuHtml(httpRequest, null, userState.getSubmitID(), STYLE_HOME);
        menuHtml.addTo(html, false, "m", Collections.singletonList(menu));
        // content
        final int lineWidth = NumberU.toInt(
                page.getProperties().getProperty(App.Action.HEX_VIEW_PARAM), NumberU.Const.RADIX_HEX);
        final HexRenderer renderer = new HexRenderer(lineWidth);
        final String text = renderer.render(bytes, start, finish);
        ElementU.addElement(html, Html.PRE, text);
        return null;
    }

    private static final String STYLE_HOME = "background-color: brown; color: white;";
}
