package io.github.greyp9.arwo.app.jdbc.sh.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.connect.AppConnectionView;
import io.github.greyp9.arwo.app.core.view.history.AppHistoryView;
import io.github.greyp9.arwo.app.jdbc.sh.core.JDBCRequest;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.script.History;
import org.w3c.dom.Element;

import java.io.IOException;

public class JDBCInventoryXView extends JDBCView {

    public JDBCInventoryXView(final JDBCRequest request, final AppUserState userState) {
        super(request, userState);
    }

    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        final ServletHttpRequest httpRequest = getRequest().getHttpRequest();
        final Bundle bundle = getRequest().getBundle();
        final AppUserState userState = getUserState();
        final History history = userState.getJDBC().getHistory();
        new JDBCInventoryView(httpRequest, userState, "").addContent(html);
        new AppConnectionView(httpRequest, userState, userState.getInterop().getCache(), "").addContentTo(html);
        new AppHistoryView("jdbcHistoryType", true, history, bundle, httpRequest, userState).addContentTo(html); // i18n
        return null;
    }
}
