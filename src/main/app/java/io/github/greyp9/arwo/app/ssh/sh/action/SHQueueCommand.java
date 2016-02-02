package io.github.greyp9.arwo.app.ssh.sh.action;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.ssh.connection.SSHConnectionFactory;
import io.github.greyp9.arwo.app.ssh.connection.SSHConnectionResource;
import io.github.greyp9.arwo.app.ssh.sh.core.SHRequest;
import io.github.greyp9.arwo.core.alert.Alert;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.bundle.Bundle;
import io.github.greyp9.arwo.core.connect.ConnectionCache;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.script.Script;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.result.view.ResultsContext;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.vm.exec.UserExecutor;
import io.github.greyp9.arwo.core.xed.action.XedActionCommand;
import io.github.greyp9.arwo.lib.ganymed.ssh.command.runnable.ScriptContext;
import io.github.greyp9.arwo.lib.ganymed.ssh.command.runnable.ScriptRunnable;
import io.github.greyp9.arwo.lib.ganymed.ssh.connection.SSHConnection;

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
        final SSHConnectionFactory factory = new SSHConnectionFactory(httpRequest, userState, bundle, alerts);
        final ConnectionCache cacheSSH = userState.getSSH().getCache();
        final SSHConnectionResource resource = (SSHConnectionResource) cacheSSH.getResource(server, factory);
        if (resource == null) {
            alerts.add(new Alert(Alert.Severity.WARN, bundle.format("SFTPHandlerPostMultipart.no.connect", server)));
        } else {
            location = doAction(httpArguments, resource.getConnection());
        }
        return location;
    }

    private String doAction(final NameTypeValues httpArguments, final SSHConnection sshConnection) throws IOException {
        final ServletHttpRequest httpRequest = request.getHttpRequest();
        final AppUserState userState = request.getUserState();
        final String server = request.getServer();
        // queue command text for execution
        final String command = new XedActionCommand(userState.getXedFactory()).getCommand(httpArguments);
        final Script script = new Script(server, httpRequest.getDate(), command);
        userState.getSSH().getHistory().add(script);
        userState.getSSH().getProperties().setProperty(App.Settings.COMMAND, command);
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
                executorStream, resultsContext, sshConnection, sshConnection.getTerm(), pollInterval);
        final ScriptRunnable runnable = new ScriptRunnable(script, context);
        userExecutor.getRunnables().add(runnable);
        userExecutor.getExecutorCommand().execute(runnable);
        // redirect to resource for execution monitor
        final DateX dateX = DateX.Factory.createURL();
        final String commandID = dateX.toString(httpRequest.getDate());
        return PathU.toDir(httpRequest.getBaseURI(), server, commandID);
    }
}
