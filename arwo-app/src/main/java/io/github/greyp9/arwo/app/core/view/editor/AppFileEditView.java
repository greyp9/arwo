package io.github.greyp9.arwo.app.core.view.editor;

import io.github.greyp9.arwo.app.core.state.AppUserState;
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
import io.github.greyp9.arwo.core.value.NameTypeValuesU;
import io.github.greyp9.arwo.core.xed.action.XedAction;
import org.w3c.dom.Element;

import java.io.IOException;
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
            final Element html, final MetaFile metaFile, final String charset) throws IOException {
        final byte[] bytes = StreamU.read(metaFile.getBytes());
        final String fileText = UTF8Codec.toString(bytes, charset);

        final XedAction action = new XedAction(
                App.Actions.QNAME_FILE_EDIT, userState.getXedFactory(), userState.getLocale());
        action.update(NameTypeValuesU.create("fileEdit.fileEditType.file", fileText));
        action.addPropertyPageTo(html, userState.getSubmitID(), httpRequest, userState.getDocumentState(),
                CollectionU.toCollection(App.Action.FILE_UPDATE));

        final FileMetaData metaData = metaFile.getMetaData();
        final String name = new FileX(metaData.getPath()).getFilename();
        final int length = bytes.length;
        final String lastModified = HttpDateU.toHttpZ(new Date(metaData.getLastModified()));
        final String hash = HexCodec.encode(HashU.md5(bytes));
        final Bundle bundle = action.getXed().getBundle();
        final String message = bundle.format("AppFileEditView.editor", name, length, lastModified, hash);
        userState.getAlerts().add(new Alert(Alert.Severity.INFO, message));
        return null;
    }
}
