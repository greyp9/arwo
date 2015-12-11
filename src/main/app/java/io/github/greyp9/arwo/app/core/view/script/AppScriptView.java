package io.github.greyp9.arwo.app.core.view.script;

import io.github.greyp9.arwo.app.core.view.command.AppCommandView;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.io.command.Command;
import io.github.greyp9.arwo.core.io.script.Script;
import org.w3c.dom.Element;

import java.io.IOException;

public class AppScriptView {
    private final Script script;

    public AppScriptView(final Script script) {
        this.script = script;
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    public final HttpResponse addContentTo(final Element html) throws IOException {
        for (final Command command : script.getCommands()) {
            new AppCommandView(command).addContentTo(html);
        }
        return null;
    }
}
