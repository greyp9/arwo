package io.github.greyp9.arwo.app.local.sh.action;

import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.local.sh.core.SHRequest;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.command.local.ScriptContext;
import io.github.greyp9.arwo.core.command.local.ScriptRunnable;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.script.History;
import io.github.greyp9.arwo.core.io.script.Script;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.result.view.ResultsContext;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.vm.exec.UserExecutor;
import io.github.greyp9.arwo.core.xed.action.XedActionCommand;

import java.io.File;
import java.io.IOException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;

public class SHQueueCommand {
    private final SHRequest request;

    public SHQueueCommand(final SHRequest request) {
        this.request = request;
    }

    public final String doAction(final NameTypeValues httpArguments) throws IOException {
        final ServletHttpRequest httpRequest = request.getHttpRequest();
        final AppUserState userState = request.getUserState();
        // queue command text for execution
        final String command = new XedActionCommand(userState.getXedFactory()).getCommand(httpArguments);
        final History history = userState.getLocal().getHistory();
        final String id = history.getNewID(httpRequest.getDate());
        final Script script = new Script(request.getContext(), httpRequest.getDate(), id, command);
        history.add(script);
        userState.getProperties().setProperty(App.Settings.COMMAND, command);
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
                executorStream, resultsContext, pollInterval, new File(SystemU.userHome()));
        final ScriptRunnable runnable = new ScriptRunnable(script, context);
        userExecutor.getRunnables().add(runnable);
        final Future<?> future = userExecutor.getExecutorCommand().submit(runnable);
        userExecutor.getFutures().put(id, future);
        // redirect to resource for execution monitor
        final DateX dateX = DateX.Factory.createFilenameMilli();
        final String commandID = dateX.toString(httpRequest.getDate());
        return PathU.toDir(httpRequest.getBaseURI(), request.getContext(), commandID);
    }
}
