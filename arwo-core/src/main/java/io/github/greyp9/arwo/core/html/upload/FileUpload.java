package io.github.greyp9.arwo.core.html.upload;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xpath.XPather;
import org.w3c.dom.Element;

import java.io.IOException;

public class FileUpload {
    // private final String servletPath;
    private final Bundle bundle;

    public FileUpload(final String ignored, final Bundle bundle) {
        // this.servletPath = servletPath;
        this.bundle = bundle;
    }

/*
    public final HttpResponse addContentTo(final Element html, final MenuView menuView) throws IOException {
        final MenuItem itemRoot = menuView.getMenuSystem().get(servletPath, AppMenuFactory.Const.FILESYSTEM);
        final MenuItem itemFile = itemRoot.getMenuItem(Const.FILE);
        final MenuItem itemUpload = itemFile.getMenuItem(Const.UPLOAD);
        final boolean isOpen = itemRoot.isOpen() && itemFile.isOpen() && itemUpload.isOpen();
        return addContentTo(html, isOpen);
    }
*/

    public final HttpResponse addContentTo(final Element html, final String isOpen) throws IOException {
        return addContentTo(html, Boolean.parseBoolean(isOpen));
    }

    public final HttpResponse addContentTo(final Element html, final boolean isOpen) throws IOException {
        // find the document insert point for the upload menu
        final Element divMenuFileUpload = new XPather(html.getOwnerDocument(), null)
                .getElement("/html/body//div[button/text()[contains(.,'Upload')]]");
        if ((divMenuFileUpload != null) && isOpen) {
            final String labelMenu = bundle.getString(Const.MENU_FILE_UPLOAD);
            final Element parent = ElementU.getParent(ElementU.getParent(ElementU.getParent(divMenuFileUpload)));
            final Element form = ElementU.addElement(parent, Html.FORM, null, NTV.create(
                    Html.METHOD, Html.POST, Html.ACTION, Html.EMPTY, Html.ENCTYPE, Http.Mime.FORM_MULTIPART));
            final Element divMenu = ElementU.addElement(form, Html.DIV, null, NTV.create(Html.CLASS, App.CSS.MENU));
            ElementU.addElement(divMenu, Html.SPAN, String.format("[%s]", labelMenu), NTV.create(
                    Html.CLASS, App.CSS.MENU));
            ElementU.addElement(divMenu, Html.INPUT, null, NTV.create(Html.TYPE, Html.FILE,
                    Html.NAME, App.Post.UPLOAD_FILE, Html.SIZE, Const.SIZE, Html.STYLE, Const.STYLE));
            ElementU.addElement(divMenu, Html.INPUT, null, NTV.create(
                    Html.TYPE, Html.SUBMIT, Html.NAME, Const.SUBMIT, Html.VALUE, labelMenu));
        }
        return null;
    }

    private static class Const {
        // private static final String FILE = "file";  // i18n internal
        // private static final String UPLOAD = "upload";  // i18n internal
        private static final String SIZE = "80";  // i18n internal
        private static final String STYLE = "padding: 0 1em;";  // i18n internal
        private static final String SUBMIT = "submitUploadFile";  // i18n internal
        private static final String MENU_FILE_UPLOAD = "menu.file.upload";
    }
}
