package io.github.greyp9.arwo.app.interop.action;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.interop.connection.InteropConnectionFactory;
import io.github.greyp9.arwo.app.interop.connection.InteropConnectionResource;
import io.github.greyp9.arwo.app.interop.core.SHRequest;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.connect.ConnectionCache;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.script.Script;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.result.io.ResultsPersister;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.vm.exec.UserExecutor;
import io.github.greyp9.arwo.core.xed.action.XedActionCommand;
import io.github.greyp9.arwo.lib.interop.dcom.command.runnable.ScriptContext;
import io.github.greyp9.arwo.lib.interop.dcom.command.runnable.ScriptRunnable;
import io.github.greyp9.arwo.lib.interop.dcom.connection.InteropConnection;

import java.io.File;
import java.io.IOException;
import java.util.concurrent.ExecutorService;

public class SHQueueCommand {
    private final SHRequest request;

    public SHQueueCommand(final SHRequest request) {
        this.request = request;
    }

    public final String doAction(final String locationIn, final NameTypeValues httpArguments) throws IOException {
        String location = locationIn;
        final ServletHttpRequest httpRequest = request.getHttpRequest();
        final AppUserState userState = request.getUserState();
        final Bundle bundle = request.getBundle();
        final Alerts alerts = request.getAlerts();
        final String server = request.getServer();
        final InteropConnectionFactory factory = new InteropConnectionFactory(httpRequest, userState, bundle, alerts);
        final ConnectionCache cache = userState.getInterop().getCache();
        final InteropConnectionResource resource = (InteropConnectionResource) cache.getResource(server, factory);
        if (resource == null) {
            alerts.add(new Alert(Alert.Severity.WARN, bundle.format("SFTPHandlerPostMultipart.no.connect", server)));
        } else {
            location = doAction(httpArguments, resource.getConnection());
        }
        return location;
    }

    private String doAction(final NameTypeValues httpArguments, final InteropConnection connection) throws IOException {
        final ServletHttpRequest httpRequest = request.getHttpRequest();
        final AppUserState userState = request.getUserState();
        final Locus locus = userState.getLocus();
        final Alerts alerts = request.getAlerts();
        final String server = request.getServer();
        // queue command text for execution
        final String command = new XedActionCommand(request.getLocale()).getCommand(httpArguments);
        final Script script = new Script(server, httpRequest.getDate(), command);
        userState.getInterop().getHistory().add(script);
        userState.getInterop().getProperties().setProperty("command", command);
        // runnable to execute commands
        final UserExecutor userExecutor = userState.getUserExecutor();
        final ExecutorService executorStream = userExecutor.getExecutorStream();
        final long pollInterval = DurationU.toMillis("PT0.010S");
        final File fileResult = new ResultsPersister(request.getAppRequest()).getFile(userState.getUserRoot());
        final ScriptContext context = new ScriptContext(
                connection, executorStream, fileResult, locus, alerts, pollInterval);
        final ScriptRunnable runnable = new ScriptRunnable(context, script);
        userExecutor.getRunnables().add(runnable);
        userExecutor.getExecutorCommand().execute(runnable);
        // redirect to resource for execution monitor
        final DateX dateX = DateX.Factory.createURL();
        final String commandID = dateX.toString(httpRequest.getDate());
        return PathU.toDir(httpRequest.getBaseURI(), server, commandID);
    }
}
