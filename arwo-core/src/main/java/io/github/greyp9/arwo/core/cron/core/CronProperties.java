package io.github.greyp9.arwo.core.cron.core;

import java.util.Properties;

// i18nf
public final class CronProperties {

    private CronProperties() {
    }

    public static Properties getProperties() {
        final Properties properties = new Properties();
        properties.setProperty("arguments", "io.github.greyp9.arwo.core.cron.instance.ArgumentsRunnable");
        properties.setProperty("sleep", "io.github.greyp9.arwo.core.cron.instance.SleepRunnable");

        properties.setProperty("lsh", "io.github.greyp9.arwo.app.local.sh.cron.SHRunnable");
        properties.setProperty("ssh", "io.github.greyp9.arwo.app.ssh.sh.cron.SHRunnable");
        properties.setProperty("wsh", "io.github.greyp9.arwo.app.interop.sh.cron.SHRunnable");
        properties.setProperty("jdbc", "io.github.greyp9.arwo.app.jdbc.sh.cron.JDBCRunnable");

        properties.setProperty("sftp", "io.github.greyp9.arwo.app.ssh.sftp.cron.SFTPRunnable");
        properties.setProperty("cifs", "io.github.greyp9.arwo.app.cifs.fs.cron.CIFSRunnable");
        properties.setProperty("webdav", "io.github.greyp9.arwo.app.webdav.fs.cron.WebDAVRunnable");

        properties.setProperty("smtp", "io.github.greyp9.arwo.app.mail.smtp.cron.SMTPRunnable");
        properties.setProperty("imap", "io.github.greyp9.arwo.app.mail.imap.cron.IMAPRunnable");
        properties.setProperty("pop3", "io.github.greyp9.arwo.app.mail.pop3.cron.POP3Runnable");
        return properties;
    }
}
