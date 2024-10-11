package io.github.greyp9.arwo.core.io.script.read;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.date.DateX;
import io.github.greyp9.arwo.core.date.HttpDateU;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.io.script.Script;

import java.io.IOException;
import java.util.Date;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ScriptReader {
    private final DateX dateX;

    public ScriptReader() {
        this.dateX = DateX.Factory.createFilenameMilli();
    }

    public final Script readFrom(final String filename, final byte[] bytes) throws IOException {
        final String dateString = filename.replace(".results", "");
        final Date dateId = dateX.toDate(dateString);
        final String commandId = dateX.toString(dateId);
        final String textResult = UTF8Codec.toString(bytes);
        final Matcher matcher = PATTERN_SCRIPT.matcher(textResult);
        if (matcher.matches()) {
            final Date start = HttpDateU.fromHttpZMilli(matcher.group(1));
            final Date finish = HttpDateU.fromHttpZMilli(matcher.group(9));
            final String context = matcher.group(2);
            final String commands = matcher.group(3);
            final StringBuilder buffer = new StringBuilder();
            final Matcher matcherCommand = PATTERN_COMMAND.matcher(commands);
            while (matcherCommand.find()) {
                //final String dir = matcherCommand.group(1);
                final String stdin = matcherCommand.group(2);
                //final String stderr = matcherCommand.group(3);
                //final String stdout = matcherCommand.group(4);
                buffer.append(stdin.trim()).append(Http.Token.LF);
            }
            final Script script = new Script(context, dateId, commandId, buffer.toString());
            script.deserialize(start, finish);
            return script;
        } else {
            return null;
        }
    }

    private static final String REGEX_SCRIPT
            = "(?s)(.+?)\\s+"
            + "-{76}\\s+"
            + "(.+?)\\s+"
            + "-{76}\\s+"
            + "(("
            + "DIR\\s+(.+?)\\s+"
            + "STDIN\\s+(.*?)\\s+"
            + "STDERR\\s+(.*?)\\s+"
            + "STDOUT\\s+(.*?)\\s+"
            + "-{76}\\s+"
            + ")+)"
            + "(.+?)\\s+?";

    private static final String REGEX_COMMAND
            = "(?sm)"
            + "^DIR$"
            + "(.*?)"
            + "^STDIN$"
            + "(.*?)"
            + "^STDERR$"
            + "(.*?)"
            + "^STDOUT$"
            + "(.*?)"
            + "-{76}\\s+";

    private static final Pattern PATTERN_SCRIPT = Pattern.compile(REGEX_SCRIPT);
    private static final Pattern PATTERN_COMMAND = Pattern.compile(REGEX_COMMAND);
}
