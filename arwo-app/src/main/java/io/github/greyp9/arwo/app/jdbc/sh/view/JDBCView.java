package io.github.greyp9.arwo.app.jdbc.sh.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.fixup.AppHtmlView;
import io.github.greyp9.arwo.app.jdbc.sh.core.JDBCRequest;
import io.github.greyp9.arwo.core.app.menu.AppMenuFactory;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.menu.MenuContext;
import io.github.greyp9.arwo.core.menu.MenuItem;
import io.github.greyp9.arwo.core.menu.MenuSystem;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xpath.XPather;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Collections;
import java.util.Optional;

@SuppressWarnings({ "PMD.AbstractNaming", "PMD.ExcessiveImports" })
public abstract class JDBCView {
    private final JDBCRequest request;
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final MenuContext menuContext;

    public final JDBCRequest getRequest() {
        return request;
    }

    public final AppUserState getUserState() {
        return userState;
    }

    public JDBCView(final JDBCRequest request, final AppUserState userState) {
        this.request = request;
        this.httpRequest = request.getHttpRequest();
        this.userState = userState;

        final MenuSystem menuSystem = userState.getMenuSystem();
        final MenuItem menuItem = menuSystem.get(httpRequest.getServletPath(), AppMenuFactory.Const.COMMAND);
        this.menuContext = new MenuContext(menuSystem, Collections.singletonList(menuItem));
    }

    public final HttpResponse doGetResponse() throws IOException {
        final Document html = DocumentU.toDocument(StreamU.read(userState.getXHTML()));
        final Element body = new XPather(html, null).getElement(Html.XPath.CONTENT);
        final HttpResponse httpResponse = addContentTo(body);
        return Optional.ofNullable(httpResponse)
                .orElse(new AppHtmlView(httpRequest, userState, menuContext).fixup(html));
    }

    protected abstract HttpResponse addContentTo(Element html) throws IOException;
}
