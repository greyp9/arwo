package io.github.greyp9.arwo.app.local.sh2.handler;

import io.github.greyp9.arwo.app.core.handler.AppHandlerPost;
import io.github.greyp9.arwo.app.core.state.AppUserState;
import io.github.greyp9.arwo.app.local.sh2.core.CommandLSH;
import io.github.greyp9.arwo.app.local.sh2.runnable.RunnableLSH;
import io.github.greyp9.arwo.core.http.servlet.ServletHttpRequest;
import io.github.greyp9.arwo.core.submit.SubmitToken;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.vm.mutex.CollectionU;
import io.github.greyp9.arwo.core.xed.action.XedActionCommand;

import java.io.IOException;
import java.util.concurrent.ExecutorService;

public final class SHHandlerPost extends AppHandlerPost {

    public SHHandlerPost(final ServletHttpRequest httpRequest, final AppUserState userState) {
        super(httpRequest, userState);
    }

    protected String applySession(
            final SubmitToken token, final NameTypeValues httpArguments, final String locationIn) throws IOException {
        final AppUserState userState = getUserState();
        final String command = new XedActionCommand(userState.getXedFactory()).getCommand(httpArguments);
        final CommandLSH commandSH = new CommandLSH(getHttpRequest().getDate(), command);
        CollectionU.add(userState.getLSH().getCommands(), commandSH);
        final ExecutorService executor = userState.getUserExecutor().getExecutorCommand();
        executor.submit(new RunnableLSH(commandSH));
        return locationIn;
    }
}
