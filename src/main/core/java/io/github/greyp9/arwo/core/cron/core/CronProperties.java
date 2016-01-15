package io.github.greyp9.arwo.core.cron.core;

import java.util.Properties;

public final class CronProperties {

    private CronProperties() {
    }

    public static Properties getProperties() {
        final Properties properties = new Properties();
        properties.setProperty("arguments", "io.github.greyp9.arwo.core.cron.instance.ArgumentsRunnable");  // i18n
        properties.setProperty("sleep", "io.github.greyp9.arwo.core.cron.instance.SleepRunnable");  // i18n

        properties.setProperty("lsh", "io.github.greyp9.arwo.app.local.sh.cron.SHRunnable");  // i18n
        properties.setProperty("ssh", "io.github.greyp9.arwo.app.ssh.sh.cron.SHRunnable");  // i18n
        properties.setProperty("wsh", "io.github.greyp9.arwo.app.interop.sh.cron.SHRunnable");  // i18n
        properties.setProperty("jdbc", "io.github.greyp9.arwo.app.jdbc.sh.cron.JDBCRunnable");  // i18n

        properties.setProperty("sftp", "io.github.greyp9.arwo.app.ssh.sftp.cron.SFTPRunnable");  // i18n
        properties.setProperty("cifs", "io.github.greyp9.arwo.app.cifs.fs.runnable.CIFSRunnable");  // i18n
        properties.setProperty("webdav", "io.github.greyp9.arwo.app.webdav.fs.runnable.WebDAVRunnable");  // i18n
        return properties;
    }
}
