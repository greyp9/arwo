package io.github.greyp9.arwo.app.core.view.script;

import io.github.greyp9.arwo.app.core.view.command.AppCommandView;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.io.command.Command;
import io.github.greyp9.arwo.core.io.script.Script;
import io.github.greyp9.arwo.core.locus.Locus;
import org.w3c.dom.Element;

import java.io.IOException;

public class AppScriptView {
    private final Script script;
    private final Locus locus;

    public AppScriptView(final Script script, final Locus locus) {
        this.script = script;
        this.locus = locus;
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    public final HttpResponse addContentTo(final Element html) throws IOException {
        for (final Command command : script.getCommands()) {
            new AppCommandView(command, locus).addContentTo(html);
        }
        return null;
    }
}
