package io.github.greyp9.arwo.app.vis.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.fixup.AppHtmlView;
import io.github.greyp9.arwo.app.vis.core.VisualizationRequest;
import io.github.greyp9.arwo.app.vis.menu.MenuVis;
import io.github.greyp9.arwo.core.app.AppTitle;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.menu2.model.MenuItem;
import io.github.greyp9.arwo.core.menu2.view.MenuHtml;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xpath.XPather;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Collections;
import java.util.Optional;

public abstract class VisualizationView {
    private final ServletHttpRequest httpRequest;
    private final VisualizationRequest request;
    private final AppUserState userState;
    private final Bundle bundle;
    private final Locus locus;

    public VisualizationView(final ServletHttpRequest httpRequest,
                             final VisualizationRequest request, final AppUserState userState) {
        this.httpRequest = httpRequest;
        this.request = request;
        this.userState = userState;
        this.bundle = request.getAppRequest().getBundle();
        this.locus = request.getAppRequest().getLocus();
    }

    public final ServletHttpRequest getHttpRequest() {
        return httpRequest;
    }

    public final VisualizationRequest getRequest() {
        return request;
    }

    public final AppUserState getUserState() {
        return userState;
    }

    public final Bundle getBundle() {
        return bundle;
    }

    public final Locus getLocus() {
        return locus;
    }

    public final HttpResponse doGetResponse() throws IOException {
        final Document html = DocumentU.toDocument(StreamU.read(userState.getXHTML()));
        final Element header = new XPather(html, null).getElement(Html.XPath.HEADER);
        final Element content = new XPather(html, null).getElement(Html.XPath.CONTENT);
        final Element footer = new XPather(html, null).getElement(Html.XPath.FOOTER);

        final MenuItem menu = new MenuVis().toMenuItem().applyFrom(userState.getMenuSystemState());
        final MenuHtml menuHtml = new MenuHtml(httpRequest, userState.getBundle(), userState.getSubmitID(), STYLE_HOME);
        menuHtml.addTo(header, true, "m", Collections.singletonList(menu));

        final MenuItem menuNav = getMenuNav();
        if (menuNav != null) {
            menuNav.setOpen(true);
            menuHtml.addTo(header, false, "n", Collections.singletonList(menuNav));
        }

        final AppTitle title = AppTitle.Factory.getHostLabel(httpRequest, userState.getBundle(), request.getContext());
        final HttpResponse httpResponse = addContentTo(content);
        return Optional.ofNullable(httpResponse).orElse(
                new AppHtmlView(httpRequest, userState, title)
                        .title(header)
                        .actionTextExpression(header)
                        .alerts(header)
                        .statusBar(footer)
                        .appHtml(html)
                        .toHttpResponse(html));
    }

    private static final String STYLE_HOME = "background-color: brown; color: white;";

    /**
     * Insert content into HTML page appropriate to the context of the subclass.
     *
     * @param html the wrapper HTML document
     * @return the http response containing the formatted content to be served to the requester
     * @throws IOException on failures accessing requested resources
     */
    protected abstract HttpResponse addContentTo(Element html) throws IOException;

    protected abstract MenuItem getMenuNav();
}
