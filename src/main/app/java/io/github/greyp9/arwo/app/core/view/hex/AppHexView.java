package io.github.greyp9.arwo.app.core.view.hex;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.hex.HexRenderer;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.lang.NumberU;
import io.github.greyp9.arwo.core.page.Page;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Element;

import java.io.IOException;

public class AppHexView {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;

    public AppHexView(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.httpRequest = httpRequest;
        this.userState = userState;
    }

    public final HttpResponse addContentTo(
            final Element html, final MetaFile metaFile) throws IOException {
        httpRequest.getClass();
        final byte[] bytes = StreamU.read(metaFile.getBytes());
        userState.setPageViewHex(Page.Factory.fixPage(userState.getPageViewHex(), bytes.length));
        final Page page = userState.getPageViewHex();
        final int lineWidth = NumberU.toInt(page.getProperties().getProperty("viewHex"), NumberU.Const.RADIX_HEX);
        final HexRenderer renderer = new HexRenderer(lineWidth);
        final String text = renderer.render(bytes, page.getPosition(), page.getPosition() + page.getCount());
        ElementU.addElement(html, Html.PRE, text);
        return null;
    }
}