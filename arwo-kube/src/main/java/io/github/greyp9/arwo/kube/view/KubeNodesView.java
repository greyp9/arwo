package io.github.greyp9.arwo.kube.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.table.UserStateTable;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.cache.CacheRowSetSource;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.table.row.RowSetSource;
import io.github.greyp9.arwo.kube.connection.KubeConnectionResource;
import io.github.greyp9.arwo.kube.data.NodeRowSetSource;
import io.kubernetes.client.openapi.ApiException;
import io.kubernetes.client.openapi.apis.CoreV1Api;
import org.w3c.dom.Element;

import java.io.IOException;

public class KubeNodesView extends KubeView {

    public KubeNodesView(final ServletHttpRequest httpRequest,
                         final AppUserState userState,
                         final KubeConnectionResource resource) {
        super(httpRequest, userState, resource);
    }

    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        final KubeConnectionResource resource = getResource();
        final CoreV1Api api = resource.getConnection().getCoreV1Api();
        final String baseURI = getHttpRequest().getBaseURI();
        final String endpoint = getResource().getName();
        final String rowSetId = getHttpRequest().getURI();
        final RowSetSource rowSetSource = new CacheRowSetSource(getUserState().getCache(),
                new NodeRowSetSource(api, baseURI, endpoint), rowSetId);
        try {
            final RowSet rowSet = rowSetSource.getRowSet();
            final UserStateTable table = new UserStateTable(getUserState(), null, getHttpRequest().getDate(), true);
            table.toTableView(rowSet).addContentTo(html);
        } catch (final ApiException e) {
            getUserState().getAlerts().add(new Alert(Alert.Severity.ERR, e.getResponseBody()));
        } catch (final Exception e) {
            getUserState().getAlerts().add(new Alert(Alert.Severity.ERR, e.getMessage()));
        }
        return null;
    }
}
