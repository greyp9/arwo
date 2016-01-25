package io.github.greyp9.arwo.app.core.view.command;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.glyph.UTF16;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.http.HttpResponse;
import io.github.greyp9.arwo.core.io.command.Command;
import io.github.greyp9.arwo.core.locus.Locus;
import io.github.greyp9.arwo.core.number.NumberScale;
import io.github.greyp9.arwo.core.text.render.TextRenderer;
import io.github.greyp9.arwo.core.value.NTV;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Element;

import java.io.IOException;
import java.util.Date;

public class AppCommandView {
    private final Command command;
    private final DateX dateX;

    public AppCommandView(final Command command, final Locus locus) {
        this.command = command;
        this.dateX = locus.getDateX();
    }

    public final HttpResponse addContentTo(final Element html) throws IOException {
        final Long elapsed = DurationU.toDuration(command.getStart(), command.getFinish(), new Date());
        final Element divCommand = ElementU.addElement(html, Html.DIV, null, NTV.create(Html.CLASS, App.CSS.COMMAND));
        renderHeader(divCommand, command.getStart(), command.getStdin());
        renderBody(divCommand);
        renderFooter(divCommand, command.getFinish(), elapsed);
        return null;
    }

    private void renderHeader(final Element html, final Date dateStart, final String stdin) throws IOException {
        final String dateText = ((dateStart == null) ? null : String.format("[@%s]", dateX.toString(dateStart)));
        final String stdinText = ((stdin == null) ? null : String.format("$ %s", stdin));
        final String text = Value.join(Html.SPACE, dateText, stdinText);
        ElementU.addElement(html, Html.DIV, text, NTV.create(Html.CLASS, App.CSS.COMMAND_HEAD));
    }

    private void renderBody(final Element html) throws IOException {
        final Element divB = ElementU.addElement(html, Html.DIV, null, NTV.create(Html.CLASS, App.CSS.COMMAND_BODY));
        final Element divR = ElementU.addElement(divB, Html.DIV, null, NTV.create(Html.CLASS, App.CSS.TEXT_RESULT));
        // stderr
        final TextRenderer rendererStderr = new TextRenderer(command.getStderr());
        final String cssStderr = Value.join(Html.SPACE, App.CSS.TEXT_RESULT_BODY, App.CSS.STDERR);
        final Element divStderr = ElementU.addElement(divR, Html.DIV, null, NTV.create(Html.CLASS, cssStderr));
        toOutputText(divStderr, rendererStderr.render(TextRenderer.Const.SCROLLBACK_LINES));
        // stdout
        final TextRenderer rendererStdout = new TextRenderer(command.getStdout());
        final String cssStdout = Value.join(Html.SPACE, App.CSS.TEXT_RESULT_BODY, App.CSS.STDOUT);
        final Element divStdout = ElementU.addElement(divR, Html.DIV, null, NTV.create(Html.CLASS, cssStdout));
        toOutputText(divStdout, rendererStdout.render(TextRenderer.Const.SCROLLBACK_LINES));
        // extra info
        final String cssExtra = Value.join(Html.SPACE, App.CSS.TEXT_RESULT_BODY, App.CSS.EXTRA);
        final Element divExtra = ElementU.addElement(divR, Html.DIV, null, NTV.create(Html.CLASS, cssExtra));
        toOutputText(divExtra, toExtraInfo(rendererStderr, rendererStdout));
    }

    private void renderFooter(final Element html, final Date dateFinish, final Long elapsed) throws IOException {
        final String dateText = ((dateFinish == null) ? null : String.format("[@%s]", dateX.toString(dateFinish)));
        final String elapsedText = (elapsed == null) ? null : String.format("[%s]", DurationU.durationXSD(elapsed));
        final String text = Value.defaultOnEmpty(Value.join(Html.SPACE, dateText, elapsedText), UTF16.PAUSE);
        ElementU.addElement(html, Html.DIV, text, NTV.create(Html.CLASS, App.CSS.COMMAND_FOOT));
    }

    private void toOutputText(final Element div, final String text) {
        if (Value.isEmpty(text)) {
            ElementU.detach(div);
        } else {
            ElementU.addElement(div, Html.PRE, text);
        }
    }

    private String toExtraInfo(final TextRenderer rendererStderr, final TextRenderer rendererStdout) {
        final Integer pid = command.getPID();
        final String pidText = (pid == null) ? null : String.format("[PID:%s]", pid);
        final Integer exitValue = command.getExitValue();
        final String exitValueText = (exitValue == null) ? null : String.format("[EXIT:%s]", exitValue);
        final String stderrText = toExtraInfo(rendererStderr, App.CSS.STDERR);
        final String stdoutText = toExtraInfo(rendererStdout, App.CSS.STDOUT);
        return Value.defaultOnEmpty(Value.join(Html.SPACE, pidText, exitValueText, stderrText, stdoutText), null);
    }

    private String toExtraInfo(final TextRenderer textRenderer, final String label) {
        final int characterCount = textRenderer.getCharacterCount();
        final int lineCount = textRenderer.getLineCount();
        return (lineCount == 0) ? null : String.format("[%s:%s/%s]", label,
                NumberScale.toString(characterCount), NumberScale.toString(lineCount));
    }
}
