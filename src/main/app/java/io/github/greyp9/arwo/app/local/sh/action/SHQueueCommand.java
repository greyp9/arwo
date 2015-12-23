package io.github.greyp9.arwo.app.local.sh.action;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.local.sh.core.SHRequest;
import io.github.greyp9.arwo.core.alert.Alerts;
import io.github.greyp9.arwo.core.command.local.ScriptContext;
import io.github.greyp9.arwo.core.command.local.ScriptRunnable;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.script.Script;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.vm.exec.UserExecutor;
import io.github.greyp9.arwo.core.xed.action.XedActionCommand;

import java.io.File;
import java.io.IOException;
import java.util.concurrent.ExecutorService;

public class SHQueueCommand {
    private final SHRequest request;

    public SHQueueCommand(final SHRequest request) {
        this.request = request;
    }

    public final String doAction(final NameTypeValues httpArguments) throws IOException {
        final ServletHttpRequest httpRequest = request.getHttpRequest();
        final AppUserState userState = request.getUserState();
        final Locus locus = userState.getLocus();
        final Alerts alerts = request.getAlerts();
        // queue command text for execution
        final String command = new XedActionCommand(request.getLocale()).getCommand(httpArguments);
        final String host = request.getHttpRequest().getHeader(Http.Header.HOST);
        final Script script = new Script(host, httpRequest.getDate(), command);
        userState.getLocal().getHistory().add(script);
        userState.getLocal().getProperties().setProperty("command", command);
        // runnable to execute commands
        final UserExecutor userExecutor = userState.getUserExecutor();
        final ExecutorService executorStream = userExecutor.getExecutorStream();
        final long pollInterval = DurationU.toMillis("PT0.010S");
        final File userHome = new File(SystemU.userHome());
        final File folder = new File(userState.getUserRoot(), PathU.toDir("",
                Value.defaultOnEmpty(httpRequest.getHeader("X-Out"), "interactive"),
                httpRequest.getServletPath()));
        final ScriptContext context = new ScriptContext(
                executorStream, pollInterval, userHome, folder, locus, alerts);
        final ScriptRunnable runnable = new ScriptRunnable(script, context);
        userExecutor.getRunnables().add(runnable);
        userExecutor.getExecutorCommand().execute(runnable);
        // redirect to resource for execution monitor
        final DateX dateX = DateX.Factory.createURL();
        final String commandID = dateX.toString(httpRequest.getDate());
        return PathU.toDir(httpRequest.getBaseURI(), commandID);
    }
}
