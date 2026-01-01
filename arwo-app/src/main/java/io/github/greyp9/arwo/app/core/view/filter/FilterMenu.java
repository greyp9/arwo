package io.github.greyp9.arwo.app.core.view.filter;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.menu2.model.MenuItem;
import io.github.greyp9.arwo.core.menu2.view.MenuHtml;
import io.github.greyp9.arwo.core.xed.model.Xed;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Collections;

public class FilterMenu {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final String type;
    private final String xpath;
    private final String typeInstanceName;
    private final String filterType;

    public FilterMenu(final ServletHttpRequest httpRequest,
                      final AppUserState userState,
                      final String type,
                      final String xpath,
                      final String typeInstanceName,
                      final String filterType) {
        this.httpRequest = httpRequest;
        this.userState = userState;
        this.type = type;
        this.xpath = xpath;
        this.typeInstanceName = typeInstanceName;
        this.filterType = filterType;
    }

    public final void addContentTo(final Element header) throws IOException {
        final Xed xed = userState.getDocumentState().getSession(App.Servlet.FAVORITES).getXed();
        final FilterMenuFactory menuFactory = new FilterMenuFactory(xed, xpath, typeInstanceName, filterType);
        final MenuItem menu = menuFactory.create(httpRequest.getServletPath(), type, null)
                .applyFrom(userState.getMenuSystemState());
        final MenuHtml menuHtml = new MenuHtml(httpRequest, null, userState.getSubmitID(), null);
        menuHtml.addTo(header, false, null, Collections.singletonList(menu));
    }
}
