package io.github.greyp9.arwo.core.task.type.env;

public class EnvRunnable implements Runnable {

    private final EnvTask task;

    public EnvRunnable(final EnvTask task) {
        this.task = task;
    }

    @Override
    public final void run() {
        task.getEnv().put(task.getKey(), task.getValue());
    }
}
