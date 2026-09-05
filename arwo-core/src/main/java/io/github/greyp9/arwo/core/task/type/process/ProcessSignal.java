package io.github.greyp9.arwo.core.task.type.process;

import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.vm.process.ProcessU;
import io.github.greyp9.arwo.core.vm.thread.ThreadU;

import java.io.IOException;
import java.util.logging.Logger;

public final class ProcessSignal {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final Long pid;

    public ProcessSignal(final Long pid) {
        this.pid = pid;
    }

    public void sigint() throws IOException {
        logger.info(String.format("SIGINT(%d)", pid));
        final String[] commandArray = {"kill", "-2", String.valueOf(pid)};
        final Runtime runtime = Runtime.getRuntime();
        final Process process = runtime.exec(commandArray);
        Integer exitValue = null;
        while (exitValue == null) {
            ThreadU.sleepMillis(DurationU.Const.TEN_MILLIS);
            exitValue = ProcessU.isProcessFinished(process);
        }
        logger.info(String.format("SIGINT(%d):%d", pid, exitValue));
    }
}
