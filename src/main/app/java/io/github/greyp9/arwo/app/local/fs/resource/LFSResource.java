package io.github.greyp9.arwo.app.local.fs.resource;

import io.github.greyp9.arwo.app.local.fs.core.LFSRequest;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.config.CursorFolder;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xed.session.XedSession;

import java.io.File;
import java.io.IOException;

public class LFSResource {

    public static File getFolderBase(final LFSRequest request) throws IOException {
        File file;
        final XedSession session = request.getUserState().getDocumentState().getSession(App.Servlet.SETTINGS);
        final String name = request.getFolder();
        final CursorFolder cursorFolder = new CursorFolder(session.getXed(), request.getFolder());
        if (Value.isEmpty(name)) {
            file = null;
        } else if (cursorFolder.getCursor() == null) {
            file = null;
        } else {
            file = new File(cursorFolder.getFolder());
        }
        return file;
    }
}
