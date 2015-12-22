package io.github.greyp9.arwo.core.cron.core;

import java.util.Properties;

public final class CronProperties {

    private CronProperties() {
    }

    public static Properties getProperties() {
        final Properties properties = new Properties();
        properties.setProperty("arguments", "io.github.greyp9.arwo.core.cron.instance.ArgumentsRunnable");
        properties.setProperty("sleep", "io.github.greyp9.arwo.core.cron.instance.SleepRunnable");
        properties.setProperty("ssh", "io.github.greyp9.arwo.app.ssh.sh.cron.SHRunnable");
        return properties;
    }
}
