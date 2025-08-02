package io.github.greyp9.arwo.app.mail.pop3.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.mail.pop3.core.POP3Request;
import io.github.greyp9.arwo.app.mail.pop3.data.POP3DataSource;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.cache.ResourceCache;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.result.io.ResultsPersister;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.state.ViewState;
import io.github.greyp9.arwo.core.table.state.ViewStates;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import org.w3c.dom.Element;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.HttpURLConnection;

public class POP3MessageView extends POP3View {

    public POP3MessageView(final POP3Request request, final AppUserState userState) {
        super(request, userState);
    }

    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        final POP3Request request = getRequest();
        final Bundle bundle = request.getBundle();
        final Locus locus = request.getLocus();
        final AppUserState userState = request.getUserState();
        final ViewStates viewStates = userState.getViewStates();
        final RowSetMetaData metaDataFolders = POP3DataSource.getFoldersMetaData();
        final RowSetMetaData metaDataMessages = POP3DataSource.getMessagesMetaData();
        final ViewState viewStateFolders = viewStates.getViewState(metaDataFolders, bundle, locus);
        final ViewState viewStateMessages = viewStates.getViewState(metaDataMessages, bundle, locus);
        final boolean isConnected = (viewStateFolders.isConnected() && viewStateMessages.isConnected());
        final MetaFile message = getMessage(isConnected);
        final NameTypeValues headers = new NameTypeValues(new NameTypeValue(
                Http.Header.CONTENT_TYPE, Http.Mime.TEXT_PLAIN_UTF8));
        final byte[] entity = StreamU.read(message.getBytes());
        return new HttpResponse(HttpURLConnection.HTTP_OK, headers, new ByteArrayInputStream(entity));
    }

    private MetaFile getMessage(final boolean isConnected) throws IOException {
        MetaFile message;
        final POP3Request request = getRequest();
        final AppUserState userState = request.getUserState();
        final ResourceCache cache = userState.getCacheBlob();
        final String path = request.getHttpRequest().getURI();
        if (isConnected) {
            final POP3DataSource source = getDataSource();
            message = source.getMessage(request.getFolder(), request.getMessage());
            cache.putFile(path, message);
        } else if (cache.containsRowSet(path)) {
            message = cache.getFile(path);
        } else {
            final POP3DataSource source = getDataSource();
            message = source.getMessage(request.getFolder(), request.getMessage());
            cache.putFile(path, message);
        }
        // optionally persist fetched results
        new ResultsPersister(request.getUserState().getResultsContext(
                request.getHttpRequest())).write(StreamU.read(message.getBytes()));
        // render for response
        return message;
    }
}
