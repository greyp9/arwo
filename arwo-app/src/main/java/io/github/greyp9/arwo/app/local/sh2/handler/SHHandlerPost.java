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
import io.github.greyp9.arwo.core.resource.PathU;
import io.github.greyp9.arwo.core.resource.Pather;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.vm.mutex.CollectionU;
import io.github.greyp9.arwo.core.xed.action.XedActionCommand;

import java.io.File;
import java.io.IOException;

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
        final String action = token.getAction();
        if (App.Action.SELECT_FAV.equals(action)) {
            new SHSelectFavorite(getHttpRequest(), getUserState()).doAction(token);
            return locationIn;
        }

        final AppUserState userState = getUserState();
        final Pather patherContext = new Pather(getHttpRequest().getPathInfo());
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
        return PathU.toDir(getHttpRequest().getBaseURI(), context, scriptID);
    }
}
