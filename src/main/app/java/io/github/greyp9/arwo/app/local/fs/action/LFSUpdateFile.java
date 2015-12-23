package io.github.greyp9.arwo.app.local.fs.action;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.local.fs.core.LFSRequest;
import io.github.greyp9.arwo.app.local.fs.data.LFSDataSource;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.codec.hex.HexCodec;
import io.github.greyp9.arwo.core.file.FileX;
import io.github.greyp9.arwo.core.hash.secure.HashU;
import io.github.greyp9.arwo.core.io.ByteU;
import io.github.greyp9.arwo.core.value.NameTypeValues;

import java.io.IOException;

public class LFSUpdateFile {
    private final LFSRequest request;
    private final AppUserState userState;


    public LFSUpdateFile(final LFSRequest request) {
        this.request = request;
        this.userState = request.getUserState();

    }

    public final void apply(final NameTypeValues httpArguments) throws IOException {
        final String charset = userState.getCharset();
        //String text = new XedActionFileEdit(request.getLocale()).getFile();  // filter POST data through document
        final String content = httpArguments.getValue("fileEdit.fileEditType.file");  // just grab from POST data
        final byte[] bytes = UTF8Codec.toBytes(content, charset);
        final int length = ByteU.length(bytes);
        // put data to remote
        final FileX file = new FileX(request.getPath());
        final LFSDataSource source = new LFSDataSource(request, userState.getUserRoot());
        source.write(bytes, file.getFolder(), file.getFilename());
        // info alert
        final String hash = HexCodec.encode(HashU.md5(bytes));
        request.getAlerts().add(new Alert(Alert.Severity.INFO, request.getBundle().format(
                "SFTPHandlerPostMultipart.file.target", file.getPath(), length, hash)));
    }
}
