package io.github.greyp9.arwo.app.core.view.refresh;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.action.XedActionRefresh;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xpath.XPather;
import org.w3c.dom.Element;

import java.io.IOException;

public class AppRefreshView {
    private final AppUserState userState;

    public AppRefreshView(final AppUserState userState) {
        this.userState = userState;
    }

    public final void addContentTo(final Element html) throws IOException {
        final String refresh = userState.getProperties().getProperty(XedActionRefresh.Const.KEY);
        if (!Value.isEmpty(refresh)) {
            final Element head = new XPather(html, null).getElement(Html.XPath.HEAD);
            ElementU.addElement(head, Html.META, null, NTV.create(
                    Html.HTTP_EQUIV, App.Action.REFRESH, Html.CONTENT, refresh));
        }
    }
}
