package io.github.greyp9.arwo.app.local.sh2.handler;

import io.github.greyp9.arwo.app.core.handler.AppHandlerPost;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.local.sh.action.SHSelectFavorite;
import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.exec.AppExecutorService;
import io.github.greyp9.arwo.core.exec.script.ScriptContext;
import io.github.greyp9.arwo.core.exec.script.ScriptRunnable;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.io.buffer.ByteBuffer;
import io.github.greyp9.arwo.core.lang.StringU;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.resource.Pather;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.vm.mutex.CollectionU;
import io.github.greyp9.arwo.core.xed.action.XedActionCommand;
import io.github.greyp9.arwo.core.xed.action.XedActionStdin;

import java.io.File;
import java.io.IOException;
import java.util.Optional;

public final class SHHandlerPost extends AppHandlerPost {
    private final AppExecutorService appExecutorService;

    public SHHandlerPost(final ServletHttpRequest httpRequest,
                         final AppUserState userState,
                         final AppExecutorService appExecutorService) {
        super(httpRequest, userState);
        this.appExecutorService = appExecutorService;
    }

    protected String applySession(final SubmitToken token,
                                  final NameTypeValues httpArguments,
                                  final String locationIn) throws IOException {
        String location = locationIn;
        final Pather patherContext = new Pather(getHttpRequest().getPathInfo());
        final Pather patherScriptID = new Pather(patherContext.getRight());
        final String action = token.getAction();
        if (App.Action.SELECT_FAV.equals(action)) {
            new SHSelectFavorite(getHttpRequest(), getUserState()).doAction(token);
        } else if (App.Action.SIGNAL.equals(action)) {
            final String stdin = new XedActionStdin(getUserState().getXedFactory()).getStdin(httpArguments);
            getScriptRunnable(patherScriptID.getLeftToken()).ifPresent(r -> signal(r, stdin));
        } else if (App.Action.STDIN.equals(action)) {
            final String stdin = new XedActionStdin(getUserState().getXedFactory()).getStdin(httpArguments);
            getScriptRunnable(patherScriptID.getLeftToken()).ifPresent(r -> stdin(r, stdin));
        } else {
            final AppUserState userState = getUserState();
            final String context = Value.defaultOnEmpty(patherContext.getLeftToken(), "-");
            final String command = new XedActionCommand(userState.getXedFactory()).getCommand(httpArguments);

/* demo table; deprecated
        final CommandLSH commandSH = new CommandLSH(getHttpRequest().getDate(), command);
        CollectionU.add(userState.getLSH().getCommands(), commandSH);
        final ExecutorService executorService = userState.getUserExecutor().getExecutorCommand();
        executorService.submit(new RunnableLSH(commandSH));
*/

            final File folderPersist = userState.getLSH().getFolderPersist();
            final ScriptContext script = new ScriptContext(context, command, getHttpRequest().getDate(), folderPersist);
            CollectionU.add(userState.getLSH().getScripts(), script);
            final ScriptRunnable scriptRunnable = new ScriptRunnable(script, appExecutorService.getExecutorService());
            appExecutorService.submit(scriptRunnable);
            final String scriptID = DateX.Factory.createFilenameMilli().toString(script.getDateSubmit());
            location = PathU.toDir(getHttpRequest().getBaseURI(), context, scriptID);
        }
        return location;
    }

    private Optional<ScriptRunnable> getScriptRunnable(final String scriptID) {
        return appExecutorService.getRunnables().stream()
                .filter(r -> r instanceof ScriptRunnable)
                .map(ScriptRunnable.class::cast)
                .filter(r -> r.getContext().getID().equals(scriptID))
                .findFirst();
    }

    private static void signal(final ScriptRunnable scriptRunnable, final String stdin) {
        scriptRunnable.signal(stdin);
    }

    private static void stdin(final ScriptRunnable scriptRunnable, final String stdin) {
        final ByteBuffer byteBufferStdin = scriptRunnable.getContext().getStdin();
        byteBufferStdin.addString(StringU.unescape(stdin) + SystemU.eol());
    }
}
