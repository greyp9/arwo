package io.github.greyp9.arwo.lib.jcifs.fs.connection;

import io.github.greyp9.arwo.core.app.App;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.util.PropertiesX;
import io.github.greyp9.arwo.core.value.Value;
import jcifs.smb.NtlmPasswordAuthentication;
import jcifs.smb.SmbFile;

import java.net.MalformedURLException;
import java.util.Date;
import java.util.Properties;
import java.util.logging.Logger;

public class CIFSConnection {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final SmbFile smbFile;
    private final long dateOpen;

    private final Properties properties;
    private final PropertiesX propertiesX;

    public final SmbFile getSmbFile() {
        return smbFile;
    }

    public final SmbFile getSmbFile(final String path) throws MalformedURLException {
        return new SmbFile(Value.join("", smbFile.getPath(), path), getAuthentication());
    }

    public final NtlmPasswordAuthentication getAuthentication() {
        return (NtlmPasswordAuthentication) smbFile.getPrincipal();
    }

    public final Date getDateOpen() {
        return new Date(dateOpen);
    }

    public final Properties getProperties() {
        return properties;
    }

    public CIFSConnection(final SmbFile smbFile) {
        this.smbFile = smbFile;
        this.dateOpen = new Date().getTime();


        this.properties = new Properties();
        this.propertiesX = new PropertiesX(properties);
        propertiesX.setLong(App.Connection.DATE_LAST, dateOpen);
        propertiesX.setLong(App.Connection.COUNT, 0L);
        propertiesX.setLong(App.Connection.MILLIS, 0L);
    }

    public final Date getDateLast() {
        return new Date(propertiesX.getLong(App.Connection.DATE_LAST));
    }

    public final long getCount() {
        return propertiesX.getLong(App.Connection.COUNT);
    }

    public final long getMillis() {
        return propertiesX.getLong(App.Connection.MILLIS);
    }

    public final void update(final Date date) {
        propertiesX.setLong(App.Connection.DATE_LAST, date.getTime());
        propertiesX.addLong(App.Connection.COUNT, 1L);
        final long millis = SystemU.currentTimeMillis() - date.getTime();
        propertiesX.addLong(App.Connection.MILLIS, millis);
    }

    public final void close() {
        logger.finest("close()");  // find library hook to close connection
    }
}
