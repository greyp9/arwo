package io.github.greyp9.arwo.app.local.sh2.runnable;

import io.github.greyp9.arwo.app.local.sh2.core.CommandLSH;
import io.github.greyp9.arwo.core.date.DurationU;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.vm.thread.ThreadU;

import java.util.Date;

public final class RunnableLSH implements Runnable {
    private final CommandLSH command;

    public RunnableLSH(final CommandLSH command) {
        this.command = command;
    }

    @Override
    public void run() {
        ThreadU.sleepMillis(DurationU.Const.ONE_SECOND_MILLIS * command.getIn().length());
        command.setOut(Value.generate(command.getIn(), 2));
        command.setDateFinish(new Date());
    }
}
