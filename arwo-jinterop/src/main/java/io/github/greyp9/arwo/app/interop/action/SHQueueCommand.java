package io.github.greyp9.arwo.app.interop.action;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.interop.connection.InteropConnectionFactory;
import io.github.greyp9.arwo.app.interop.connection.InteropConnectionResource;
import io.github.greyp9.arwo.app.interop.core.SHRequest;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.connect.ConnectionCache;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.script.History;
import io.github.greyp9.arwo.core.io.script.Script;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.result.view.ResultsContext;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.vm.exec.UserExecutor;
import io.github.greyp9.arwo.core.xed.action.XedActionCommand;
import io.github.greyp9.arwo.lib.interop.dcom.command.runnable.ScriptContext;
import io.github.greyp9.arwo.lib.interop.dcom.command.runnable.ScriptRunnable;
import io.github.greyp9.arwo.lib.interop.dcom.connection.InteropConnection;

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
        final String server = request.getServer();
        // queue command text for execution
        final String command = new XedActionCommand(userState.getXedFactory()).getCommand(httpArguments);
        final History history = userState.getInterop().getHistory();
        final String id = history.getNewID(httpRequest.getDate());
        final Script script = new Script(server, httpRequest.getDate(), id, command);
        history.add(script);
        userState.getInterop().getProperties().setProperty(App.Settings.COMMAND, command);
        // runnable to execute commands
        final UserExecutor userExecutor = userState.getUserExecutor();
        final ExecutorService executorStream = userExecutor.getExecutorStream();
        final long pollInterval = DurationU.toMillis("PT0.010S");
        // set call to persist results
        final String filename = String.format("%s.results", DateX.toFilename(httpRequest.getDate()));
        httpRequest.getHttpRequest().getHeaders().add(new NameTypeValue(App.Header.RESULT, filename));
        // schedule runnable
        final ResultsContext resultsContext = userState.getResultsContext(httpRequest);
        final ScriptContext context = new ScriptContext(
                executorStream, resultsContext, connection, pollInterval);
        final ScriptRunnable runnable = new ScriptRunnable(context, script);
        userExecutor.getRunnables().add(runnable);
        userExecutor.getExecutorCommand().execute(runnable);
        // redirect to resource for execution monitor
        final DateX dateX = DateX.Factory.createURL();
        final String commandID = dateX.toString(httpRequest.getDate());
        return PathU.toDir(httpRequest.getBaseURI(), server, commandID);
    }
}
