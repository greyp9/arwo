package io.github.greyp9.arwo.app.local.sh.favorite;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.menu.AppMenuFactory;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.menu.MenuSystem;
import io.github.greyp9.arwo.core.menu.factory.MenuFactory;
import io.github.greyp9.arwo.core.menu.view.MenuView;
import io.github.greyp9.arwo.core.xed.model.Xed;
import org.w3c.dom.Element;

import java.io.IOException;

public class FavoriteMenu {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;
    private final String type;

    public FavoriteMenu(final ServletHttpRequest httpRequest, final AppUserState userState, final String type) {
        this.httpRequest = httpRequest;
        this.userState = userState;
        this.type = type;
    }

    public final String getType() {
        return type;
    }

    public final void addContentTo(final Element html) throws IOException {
        final Xed xed = userState.getDocumentState().getSession(App.Servlet.FAVORITES).getXed();
        final MenuFactory menuFactory = new FavoriteMenuFactory(xed);
        final MenuSystem menuSystem = new MenuSystem(userState.getSubmitID(), menuFactory);
        /* MenuItem menuItem = */ menuSystem.get(httpRequest.getServletPath(), AppMenuFactory.Const.COMMAND_STICKY);
        menuSystem.applyState(userState.getMenuSystemState());
        final MenuView menuView = new MenuView(null, httpRequest, menuSystem);
        menuView.addContentTo(html, AppMenuFactory.Const.COMMAND_STICKY, null, false, true, "f");
    }
}
