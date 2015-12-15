package io.github.greyp9.arwo.app.core.view.editor;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.core.action.ActionButtons;
import io.github.greyp9.arwo.core.action.ActionFactory;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.codec.hex.HexCodec;
import io.github.greyp9.arwo.core.date.HttpDateU;
import io.github.greyp9.arwo.core.file.FileX;
import io.github.greyp9.arwo.core.file.meta.FileMetaData;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.hash.secure.HashU;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.util.CollectionU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.xed.action.XedActionFileEdit;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.op.OpUpdate;
import io.github.greyp9.arwo.core.xed.request.XedRequest;
import io.github.greyp9.arwo.core.xed.view.XedPropertyPageView;
import io.github.greyp9.arwo.core.xed.view.html.PropertyPageHtmlView;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;
import org.w3c.dom.Element;

import javax.xml.namespace.QName;
import java.io.IOException;
import java.util.Collection;
import java.util.Date;

@SuppressWarnings("PMD.ExcessiveImports")
public class AppFileEditView {
    private final ServletHttpRequest httpRequest;
    private final AppUserState userState;

    public AppFileEditView(final ServletHttpRequest httpRequest, final AppUserState userState) {
        this.httpRequest = httpRequest;
        this.userState = userState;
    }

    public final HttpResponse addContentTo(
            final Element html, final MetaFile metaFile, final String encoding) throws IOException {
        final byte[] bytes = StreamU.read(metaFile.getBytes());
        final String fileText = UTF8Codec.toString(bytes, encoding);
        // command input form (prep)
        final XedActionFileEdit action = new XedActionFileEdit(userState.getLocus().getLocale());
        final NameTypeValues ntv = NameTypeValuesU.create("fileEdit.fileEditType.file", fileText);
        final XedCursor cursor = action.getCursor();
        final ValueInstance valueInstanceIn = ValueInstance.create(cursor.getTypeInstance(), ntv);
        new OpUpdate(null, action.getXed().getXsdTypes()).apply(cursor.getElement(), valueInstanceIn);
        // command input form
        final Bundle bundle = cursor.getXed().getBundle();
        final QName qname = cursor.getTypeInstance().getQName();
        final String cursorType = qname.toString();
        final String submitID = userState.getSubmitID();
        final ActionFactory factory = new ActionFactory(submitID, bundle, App.Target.SESSION, cursorType, null);
        final Collection<String> actions = CollectionU.toCollection(App.Action.FILE_UPDATE);
        final ActionButtons buttons = factory.create(qname.getLocalPart(), false, actions);
        final XedRequest xedRequest = new XedRequest(httpRequest, null, userState.getDocumentState());
        new PropertyPageHtmlView(new XedPropertyPageView(null, cursor, buttons), xedRequest).addContentTo(html);
        // info alert
        final FileMetaData metaData = metaFile.getMetaData();
        final String name = new FileX(metaData.getPath()).getFilename();
        final int length = bytes.length;
        final String lastModified = HttpDateU.toHttpZ(new Date(metaData.getLastModified()));
        final String hash = HexCodec.encode(HashU.md5(bytes));
        final String message = bundle.format("AppFileEditView.editor", name, length, lastModified, hash);
        userState.getAlerts().add(new Alert(Alert.Severity.INFO, message));
        return null;
    }
}
