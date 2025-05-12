package io.github.greyp9.arwo.kube.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.table.UserStateTable;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.table.cell.TableViewLink;
import io.github.greyp9.arwo.core.table.insert.InsertRow;
import io.github.greyp9.arwo.core.table.metadata.ColumnMetaData;
import io.github.greyp9.arwo.core.table.metadata.RowSetMetaData;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.kube.connection.KubeConnectionResource;
import io.github.greyp9.arwo.kube.core.KubeU;
import io.kubernetes.client.openapi.ApiException;
import io.kubernetes.client.openapi.apis.CoreV1Api;
import io.kubernetes.client.openapi.models.V1Container;
import io.kubernetes.client.openapi.models.V1ContainerPort;
import io.kubernetes.client.openapi.models.V1ContainerState;
import io.kubernetes.client.openapi.models.V1ContainerStatus;
import io.kubernetes.client.openapi.models.V1Pod;
import io.kubernetes.client.openapi.models.V1PodSpec;
import io.kubernetes.client.openapi.models.V1PodStatus;
import org.w3c.dom.Element;

import java.io.IOException;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

public class KubeContainersView extends KubeView {

    private final String namespace;
    private final String podName;

    public KubeContainersView(final ServletHttpRequest httpRequest, final AppUserState userState,
                              final KubeConnectionResource resource, final String namespace, final String podName) {
        super(httpRequest, userState, resource);
        this.namespace = namespace;
        this.podName = podName;
    }

    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        final KubeConnectionResource resource = getResource();
        final CoreV1Api api = resource.getConnection().getCoreV1Api();
        try {
            final V1Pod v1Pod = api.readNamespacedPod(podName, namespace, null);
            final V1PodSpec v1PodSpec = Objects.requireNonNull(v1Pod.getSpec());
            final List<V1Container> containers = v1PodSpec.getContainers();
            final List<V1Container> initContainers = v1PodSpec.getInitContainers();
            final RowSet rowSet = loadRowSet(createMetaData(), v1Pod, containers, initContainers);
            final UserStateTable table = new UserStateTable(getUserState(), null, getHttpRequest().getDate());
            table.toTableView(rowSet).addContentTo(html);
        } catch (final ApiException e) {
            getUserState().getAlerts().add(new Alert(Alert.Severity.ERR, e.getMessage()));
        }
        return null;
    }

    private RowSet loadRowSet(final RowSetMetaData metaData,
                              final V1Pod v1Pod,
                              final List<V1Container> containers,
                              final List<V1Container> initContainers) {
        final String baseURI = getHttpRequest().getHttpRequest().getResource();
        final RowSet rowSet = new RowSet(metaData, null, null);

        final V1PodStatus status = v1Pod.getStatus();
        final List<V1ContainerStatus> containerStatuses = (status == null)
                ? new ArrayList<>() : status.getContainerStatuses();
        final List<V1ContainerStatus> initContainerStatuses = (status == null)
                ? new ArrayList<>() : status.getInitContainerStatuses();
        for (final V1Container container : containers) {
            final Optional<V1ContainerStatus> v1ContainerStatus = containerStatuses.stream()
                    .filter(c -> c.getName().equals(container.getName())).findFirst();
            loadRow(rowSet, baseURI, false, v1ContainerStatus.orElse(null), container);
        }
        for (final V1Container container : initContainers) {
            final Optional<V1ContainerStatus> v1ContainerStatus = initContainerStatuses.stream()
                    .filter(c -> c.getName().equals(container.getName())).findFirst();
            loadRow(rowSet, baseURI, true, v1ContainerStatus.orElse(null), container);
        }
        return rowSet;
    }

    private RowSetMetaData createMetaData() {
        final ColumnMetaData[] columns = new ColumnMetaData[]{
                new ColumnMetaData(FIELD_SELECT, Types.VARCHAR),
                new ColumnMetaData(FIELD_NAME, Types.VARCHAR, true),
                new ColumnMetaData(FIELD_IMAGE, Types.VARCHAR),
                new ColumnMetaData(FIELD_READY, Types.VARCHAR),
                new ColumnMetaData(FIELD_STATE, Types.VARCHAR),
                new ColumnMetaData(FIELD_INIT, Types.VARCHAR),
                new ColumnMetaData(FIELD_RESTARTS, Types.INTEGER),
                new ColumnMetaData(FIELD_PORTS, Types.VARCHAR),
                new ColumnMetaData(FIELD_CREATED, Types.TIMESTAMP),
        };
        return new RowSetMetaData(TABLE_ID, columns);
    }

    private static final String TABLE_ID = "kubeContainerListType";

    private void loadRow(final RowSet rowSet, final String baseURI, final boolean isInit,
                         final V1ContainerStatus v1ContainerStatus, final V1Container container) {
        final String name = container.getName();
        final String href = PathU.toDir(baseURI, name, CONTEXT_LOGS);
        final List<V1ContainerPort> ports = Optional.ofNullable(container.getPorts()).orElse(new ArrayList<>());
        final String portsText = ports.stream()
                .map(V1ContainerPort::getContainerPort).collect(Collectors.toList()).toString();
        final Optional<V1ContainerStatus> v1ContainerStatus1 = Optional.ofNullable(v1ContainerStatus);
        final V1ContainerState state = v1ContainerStatus1.map(V1ContainerStatus::getState).orElse(null);

        final InsertRow insertRow = new InsertRow(rowSet);
        insertRow.setNextColumn(new TableViewLink(UTF16.SELECT, name, href));
        insertRow.setNextColumn(container.getName());
        insertRow.setNextColumn(container.getImage());
        insertRow.setNextColumn(Boolean.toString(v1ContainerStatus1.map(V1ContainerStatus::getReady).orElse(false)));
        insertRow.setNextColumn(toText(state));
        insertRow.setNextColumn(Boolean.toString(isInit));
        insertRow.setNextColumn(v1ContainerStatus1.map(V1ContainerStatus::getRestartCount).orElse(0));
        insertRow.setNextColumn(portsText);
        insertRow.setNextColumn(toStartupDate(state));
        rowSet.add(insertRow.getRow());
    }

    private String toText(final V1ContainerState state) {
        final String text;
        if (state == null) {
            text = null;
        } else if (state.getRunning() != null) {
            text = "Running";
        } else if (state.getTerminated() != null) {
            text = "Terminated";
        } else if (state.getWaiting() != null) {
            text = "Waiting";
        } else {
            text = null;
        }
        return text;
    }

    private Date toStartupDate(final V1ContainerState state) {
        final Date date;
        if (state == null) {
            date = null;
        } else if (state.getRunning() != null) {
            date = KubeU.toDate(state.getRunning().getStartedAt());
        } else if (state.getTerminated() != null) {
            date = KubeU.toDate(state.getTerminated().getStartedAt());
        } else {
            date = null;
        }
        return date;
    }
}
