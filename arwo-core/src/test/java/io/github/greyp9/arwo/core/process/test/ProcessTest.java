package io.github.greyp9.arwo.core.process.test;

import io.github.greyp9.arwo.core.lang.SystemU;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;
import java.util.Map;

public class ProcessTest {

    @Test
    void testProcessBuilder() throws IOException, InterruptedException {
        final ProcessBuilder processBuilder = new ProcessBuilder("ls", "-l");
        final Map<String, String> environment = processBuilder.environment();
        environment.put("FOO", "BAR");
        environment.remove("FOO2");
        environment.put("FOO2", environment.get("FOO") + "2");
        processBuilder.directory(new File(SystemU.userDir()));
        final File fileOutput = new File(SystemU.tempDir(), getClass().getSimpleName() + ".txt");
        processBuilder.redirectErrorStream(true);
        processBuilder.redirectOutput(ProcessBuilder.Redirect.appendTo(fileOutput));
        final Process process = processBuilder.start();
        Assertions.assertEquals(ProcessBuilder.Redirect.PIPE, processBuilder.redirectInput());
        Assertions.assertEquals(fileOutput, processBuilder.redirectOutput().file());
        Assertions.assertEquals(-1, process.getInputStream().read());
        final int exitCode = process.waitFor();
        Assertions.assertEquals(0, exitCode);
    }
}
