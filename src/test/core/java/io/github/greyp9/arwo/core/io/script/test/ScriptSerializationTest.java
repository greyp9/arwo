package io.github.greyp9.arwo.core.io.script.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.date.XsdDateU;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.io.command.Command;
import io.github.greyp9.arwo.core.io.script.Script;
import io.github.greyp9.arwo.core.io.script.read.ScriptReader;
import io.github.greyp9.arwo.core.res.ResourceU;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.net.URL;
import java.util.Iterator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ScriptSerializationTest {

    @Test
    public void testDeserializeScriptInvocation() throws Exception {
        final URL url = ResourceU.resolve(SCRIPT_OUT);
        Assertions.assertNotNull(url);
        final String text = UTF8Codec.toString(StreamU.read(url));
        Assertions.assertNotNull(text);
        final Pattern pattern = Pattern.compile("(?s)(.+?)"
                + "\\s+-+?"
                + "\\s+(.+?)"
                + "\\s+-+?"
                + "\\s+DIR\\s+(.+?)"
                + "\\s+STDIN\\s+(.+?)"
                + "\\s+STDERR\\s+(.*?)"
                + "\\s+STDOUT\\s+(.+?)"
                + "\\s+-+?"
                + "\\s+(.+?)"
                + "\\s+");
        final Matcher matcher = pattern.matcher(text);
        final boolean matches = matcher.matches();
        Assertions.assertTrue(matches);
        int token = 0;
        Assertions.assertEquals("Sat, 01 Jan 2000 00:00:00.000 GMT", matcher.group(++token));
        Assertions.assertEquals("localhost:8443", matcher.group(++token));
        Assertions.assertEquals("~", matcher.group(++token));
        Assertions.assertEquals("ls D*", matcher.group(++token));
        Assertions.assertEquals("", matcher.group(++token));
        Assertions.assertEquals("Desktop\nDocuments", matcher.group(++token));
        Assertions.assertEquals("Sat, 01 Jan 2000 00:00:00.000 GMT", matcher.group(++token));
    }

    @Test
    public void testDeserializeScript3Invocation() throws IOException {
        final URL url = ResourceU.resolve(SCRIPT3_OUT);
        Assertions.assertNotNull(url);
        final byte[] bytes = StreamU.read(url);
        Assertions.assertNotNull(bytes);
        final Script script = new ScriptReader().readFrom("2000-01-01T00-00-00-000Z.results", bytes);
        Assertions.assertNotNull(script);
        Assertions.assertEquals(XsdDateU.fromXSDZ("2000-01-01T00:00:00Z"), script.getStart());
        Assertions.assertEquals("localhost:8443", script.getContext());
        Assertions.assertEquals(SCRIPT3_COMMANDS_EXPECTED, script.getCommands().size());
        final Iterator<Command> commandIterator = script.getCommands().iterator();
        final Command command = commandIterator.next();
        Assertions.assertEquals("ls D*", command.getStdin());
        Assertions.assertEquals(XsdDateU.fromXSDZ("2000-01-01T00:00:00Z"), script.getFinish());
    }

    private static final String SCRIPT_OUT = "io/github/greyp9/arwo/core/script/script.out.txt";
    private static final String SCRIPT3_OUT = "io/github/greyp9/arwo/core/script/script.out3.txt";
    private static final int SCRIPT3_COMMANDS_EXPECTED = 3;
}
