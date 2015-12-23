package io.github.greyp9.arwo.core.html.upload;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.app.menu.AppMenuFactory;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.menu.MenuItem;
import io.github.greyp9.arwo.core.menu.view.MenuView;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Element;

import java.io.IOException;

public class FileUpload {
    private final String servletPath;
    private final Bundle bundle;

    public FileUpload(final String servletPath, final Bundle bundle) {
        this.servletPath = servletPath;
        this.bundle = bundle;
    }

    public final HttpResponse addContentTo(final Element html, final MenuView menuView) throws IOException {
        final MenuItem itemRoot = menuView.getMenuSystem().get(servletPath, AppMenuFactory.Const.FILESYSTEM);
        final MenuItem itemFile = itemRoot.getMenuItem("file");
        final MenuItem itemUpload = itemFile.getMenuItem("upload");
        if (itemRoot.isOpen() && itemFile.isOpen() && itemUpload.isOpen()) {
            final String labelMenu = bundle.getString("menu.file.upload");
            final Element form = ElementU.addElement(html, Html.FORM, null, NTV.create(
                    Html.METHOD, Html.POST, Html.ACTION, "", Html.ENCTYPE, Http.Mime.FORM_MULTIPART));
            final Element divMenu = ElementU.addElement(form, Html.DIV, null, NTV.create(Html.CLASS, App.CSS.MENU));
            ElementU.addElement(divMenu, Html.SPAN, String.format("[%s]", labelMenu), NTV.create(
                    Html.CLASS, App.CSS.MENU));
            ElementU.addElement(divMenu, Html.INPUT, null, NTV.create(
                    Html.TYPE, Html.FILE, Html.NAME, "uploadFile", Html.SIZE, "80", Html.STYLE, "padding: 0 1em;"));
            ElementU.addElement(divMenu, Html.INPUT, null, NTV.create(
                    Html.TYPE, Html.SUBMIT, Html.NAME, "submitUploadFile", Html.VALUE, labelMenu));
        }
        return null;
    }
}
