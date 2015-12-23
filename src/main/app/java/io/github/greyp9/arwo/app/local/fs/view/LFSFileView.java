package io.github.greyp9.arwo.app.local.fs.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.local.fs.core.LFSRequest;
import io.github.greyp9.arwo.core.http.HttpResponse;
import org.w3c.dom.Element;

import java.io.File;
import java.io.IOException;

public class LFSFileView extends LFSView {

    public LFSFileView(final LFSRequest request, final AppUserState userState, final File file) {
        super(request, userState, file);
    }

    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        return null;
    }
}
