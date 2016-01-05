package io.github.greyp9.arwo.core.cron.core;

import java.util.Properties;

public final class CronProperties {

    private CronProperties() {
    }

    public static Properties getProperties() {
        final Properties properties = new Properties();
        properties.setProperty("arguments", "io.github.greyp9.arwo.core.cron.instance.ArgumentsRunnable");  // i18n
        properties.setProperty("sleep", "io.github.greyp9.arwo.core.cron.instance.SleepRunnable");  // i18n
        properties.setProperty("ssh", "io.github.greyp9.arwo.app.ssh.sh.cron.SHRunnable");  // i18n
        return properties;
    }
}
