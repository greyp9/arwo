package io.github.greyp9.arwo.core.app;

import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xpath.XPather;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.List;
import java.util.Locale;

public class AppHtml {
    private final ServletHttpRequest httpRequest;

    public AppHtml(final ServletHttpRequest httpRequest) {
        this.httpRequest = httpRequest;
    }

    public final void fixup(final Document xhtml, final AppTitle appTitle) throws IOException {
        fixup(xhtml, appTitle, "black", "default");
    }

    public final void fixup(final Document xhtml, final AppTitle appTitle,
                            final String iconColor, final String theme) throws IOException {
        fixupTitle(xhtml, appTitle);
        fixupContext(xhtml, iconColor, theme);
    }

    private void fixupTitle(final Document xhtml, final AppTitle appTitle) throws IOException {
        final Element title = new XPather(xhtml).getElement("/html/head/title");  // i18n xpath
        ElementU.setTextContent(title, appTitle.getText());
    }

    private void fixupContext(final Document xhtml, final String iconColor, final String theme) throws IOException {
        final XPather xpather = new XPather(xhtml);
        final String[] xpaths = { "//@href", "//@src" };  // i18n xpath
        for (final String xpath : xpaths) {
            final List<Attr> attrs = xpather.getAttributes(xpath);
            for (final Attr attr : attrs) {
                //attr.setValue(attr.getValue().replace("${CONTEXT}/css", "/css-static"));  // css dev hook
                attr.setValue(attr.getValue().replace("${CONTEXT}", httpRequest.getContextPath()));  // i18n internal
                final String value = attr.getValue();
                // custom icon
                if (value.contains("app-black.ico")) {
                    attr.setValue(value.replace("black", iconColor.toLowerCase(Locale.getDefault())));
                // custom css
                } else if (value.contains("webapp.css")) {
                    if ("firefox".equals(theme)) {
                        attr.setValue(value.replace("webapp.css", "webapp.moz.css"));
                    } else if ("chrome".equals(theme)) {
                        attr.setValue(value.replace("webapp.css", "webapp.crm.css"));
                    }
                }
            }
        }
    }
}
