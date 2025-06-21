package io.github.greyp9.arwo.app.core.view.refresh;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.action.XedActionRefresh;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xpath.XPather;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Properties;

/**
 * Configure rendered HTML to <a href="https://en.wikipedia.org/wiki/Meta_refresh">auto refresh</a>.
 */
public class AppRefreshView {
    private final Properties properties;

    public AppRefreshView(final Properties properties) {
        this.properties = properties;
    }

    public final void addContentTo(final Element html) throws IOException {
        final String refresh = properties.getProperty(XedActionRefresh.Const.KEY);
        if (!Value.isEmpty(refresh)) {
            final Element head = new XPather(html, null).getElement(Html.XPath.HEAD);
            ElementU.addElement(head, Html.META, null, NTV.create(
                    Html.HTTP_EQUIV, App.Action.REFRESH, Html.CONTENT, refresh));
        }
    }
}
