package io.github.greyp9.arwo.core.io.script.write;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.file.FileU;
import io.github.greyp9.arwo.core.html.Html;
import io.github.greyp9.arwo.core.io.command.Command;
import io.github.greyp9.arwo.core.io.script.Script;
import io.github.greyp9.arwo.core.lang.StringU;
import io.github.greyp9.arwo.core.locus.Locus;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;

public class ScriptWriter {
    private final Script script;

    public ScriptWriter(final Script script) {
        this.script = script;
    }

    public final void writeTo(final File file, final Locus locus) throws IOException {
        if ((file != null) && (locus != null)) {
            FileU.ensureFolders(file.getParentFile());
            final PrintStream ps = new PrintStream(new FileOutputStream(file), true, UTF8Codec.Const.UTF8);
            try {
                writeTo(ps, locus);
            } finally {
                ps.close();
            }
        }
    }

    private void writeTo(final PrintStream ps, final Locus locus) {
        final String separator = StringU.create(76, Html.HYPHEN);
        ps.println(locus.getDateX().toString(script.getStart()));
        ps.println(separator);
        for (final Command command : script.getCommands()) {
            ps.println(separator);
            ps.println("STDIN");
            ps.println(command.getStdin());
            ps.println("STDOUT");
            ps.println(command.getStdout());
            ps.println("STDERR");
            ps.println(command.getStderr());
        }
        ps.println(separator);
        ps.println(locus.getDateX().toString(script.getFinish()));
    }
}
