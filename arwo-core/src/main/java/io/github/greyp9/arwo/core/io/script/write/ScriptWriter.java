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

// i18nf
public class ScriptWriter {
    private final Script script;
    private final Locus locus;

    public ScriptWriter(final Script script, final Locus locus) {
        this.script = script;
        this.locus = locus;
    }

    public final void writeTo(final File file) throws IOException {
        if ((file != null) && (locus != null)) {
            FileU.ensureFolders(file.getParentFile());
            final PrintStream ps = new PrintStream(new FileOutputStream(file), true, UTF8Codec.Const.UTF8);
            try {
                writeTo(ps);
            } finally {
                ps.close();
            }
        }
    }

    private void writeTo(final PrintStream ps) {
        final String separator = StringU.create(76, Html.HYPHEN);
        ps.println(locus.getDateX().toString(script.getStart()));
        ps.println(separator);
        ps.println(script.getContext());
        for (final Command command : script.getCommands()) {
            ps.println(separator);
            if (command.getDir() != null) {
                ps.println("DIR");  // i18n revisit
                ps.println(command.getDir());
            }
            ps.println("STDIN");  // i18n revisit
            ps.println(command.getStdin());
            ps.println("STDERR");  // i18n revisit
            ps.println(command.getStderr());
            ps.println("STDOUT");  // i18n revisit
            ps.println(command.getStdout());
        }
        ps.println(separator);
        ps.println(locus.getDateX().toString(script.getFinish()));
    }
}
