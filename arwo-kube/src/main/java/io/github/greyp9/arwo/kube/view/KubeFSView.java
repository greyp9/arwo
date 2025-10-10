package io.github.greyp9.arwo.kube.view;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.core.view.table.UserStateTable;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.exec.script.ProcessMonitor;
import io.github.greyp9.arwo.core.exec.script.ScriptContext;
import io.github.greyp9.arwo.core.file.meta.FileMetaData;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.file.meta.MetaFolder;
import io.github.greyp9.arwo.core.file.meta.MetaFolderRowSetSource;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.http.HttpResponseU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.shell.LS;
import io.github.greyp9.arwo.core.table.row.RowSet;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.kube.connection.KubeConnectionResource;
import io.kubernetes.client.Copy;
import io.kubernetes.client.Exec;
import io.kubernetes.client.openapi.ApiClient;
import io.kubernetes.client.openapi.ApiException;
import org.w3c.dom.Element;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.atomic.AtomicReference;
import java.util.logging.Logger;

public class KubeFSView extends KubeView {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final String namespace;
    private final String podName;
    private final String containerName;
    private final String path;

    public KubeFSView(final ServletHttpRequest httpRequest, final AppUserState userState,
                      final KubeConnectionResource resource,
                      final String namespace, final String podName, final String containerName, final String path) {
        super(httpRequest, userState, resource);
        this.namespace = namespace;
        this.podName = podName;
        this.containerName = containerName;
        this.path = path;
    }

    @Override
    protected final HttpResponse addContentTo(final Element html) throws IOException {
        final KubeConnectionResource resource = getResource();
        try {
            final ApiClient apiClient = resource.getConnection().getApiClient();
            if (!path.endsWith(Http.Token.SLASH)) {
                // treat as request for file
                final Copy copy = new Copy(apiClient);
                final InputStream inputStream = copy.copyFileFromPod(namespace, podName, containerName, path);
                final ByteArrayOutputStream bos = new ByteArrayOutputStream();
                StreamU.writeFully(inputStream, bos);
                final byte[] payload = bos.toByteArray();
                final FileMetaData metaData = new FileMetaData(null, payload.length, 0L, 0L, FileMetaData.Type.FILE);
                return HttpResponseU.to200(new MetaFile(
                        metaData, Http.Mime.TEXT_PLAIN_UTF8, new ByteArrayInputStream(payload)));
            } else {
                // treat as request for directory listing
                final Exec exec = new Exec(apiClient);
                final String[] command = {"ls", "-l", path};
                logger.info(String.format("LIST=%s", path));
                final Process process = exec.exec(namespace, podName, command, containerName, false, false);
                logger.info(String.format("PROCESS=%s", process.getClass().getName()));
                final ScriptContext scriptContext = new ScriptContext(null, null, null, null);
                final ExecutorService executorServiceStreams = getUserState().getUserExecutor().getExecutorStream();
                final AtomicReference<String> reference = new AtomicReference<>();  // wire this up to app reference?
                final ProcessMonitor processMonitor = new ProcessMonitor(
                        scriptContext, executorServiceStreams, reference, POLL_INTERVAL);
                final Integer exitValue = processMonitor.monitor(process);
                logger.info(String.format("LIST=%s, HAS-EXITED=%s EXIT-CODE=%x", path, "-", exitValue));
                final String stdout = scriptContext.getStdout().getString();
                final String stderr = scriptContext.getStderr().getString();
                if (!Value.isEmpty(stderr)) {
                    getUserState().getAlerts().add(new Alert(Alert.Severity.ERR, stderr));
                }
                // render directory listing
                final String baseURI = getHttpRequest().getHttpRequest().getResource();
                final MetaFolder metaFolder = new LS().toMetaFolder(baseURI, stdout);
                final MetaFolderRowSetSource rowSetSource = new MetaFolderRowSetSource(CONTEXT_KFS, metaFolder);
                final RowSet rowSet = rowSetSource.getRowSet();
                final UserStateTable table = new UserStateTable(getUserState(), null, getHttpRequest().getDate());
                table.toTableView(rowSet).addContentTo(html);
            }
        } catch (final ApiException e) {
            getUserState().getAlerts().add(new Alert(Alert.Severity.ERR, e.getMessage()));
        }
        return null;
    }

    private static final long POLL_INTERVAL = 100L;
}
