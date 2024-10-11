package io.github.greyp9.arwo.core.view;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.date.DateU;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.vm.app.ManifestU;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Element;

import java.security.Principal;
import java.util.Date;
import java.util.Optional;

public class StatusBarView {
    private final ServletHttpRequest httpRequest;
    private final Locus locus;

    public StatusBarView(final ServletHttpRequest httpRequest, final Locus locus) {
        this.httpRequest = httpRequest;
        this.locus = locus;
    }

    public final void addContentTo(final Element html) {
        final DateX dateX = locus.getDateX();
        final Date date = httpRequest.getDate();
        final String dateString = dateX.toString(date);
        final String user = Optional.ofNullable(httpRequest.getPrincipal()).map(Principal::getName).orElse(Html.HYPHEN);
        final String duration = DurationU.durationXSD(DateU.since(date));
        final String status = String.format("[%s] [%s] [%s]", user, dateString, duration);
        final String version = ManifestU.getSpecificationVersion(getClass());
        final String versionDetail = ManifestU.getImplementationVersion(getClass());
        final Element divMenus = ElementU.addElement(html, Html.DIV, null, NTV.create(Html.CLASS, App.CSS.MENUS));
        final Element divToolbar = ElementU.addElement(divMenus, Html.DIV, null, NTV.create(Html.CLASS, App.CSS.MENU));
        final Element divR = ElementU.addElement(divToolbar, Html.DIV, null, NTV.create(Html.CLASS, App.CSS.RIGHT));
        final Element divL = ElementU.addElement(divToolbar, Html.DIV, null);
        ElementU.addElement(divL, Html.SPAN, status, NTV.create(Html.CLASS, App.CSS.MENU));
        ElementU.addElement(divR, Html.SPAN, version, NTV.create(Html.CLASS, App.CSS.MENU, Html.TITLE, versionDetail));
    }
}
