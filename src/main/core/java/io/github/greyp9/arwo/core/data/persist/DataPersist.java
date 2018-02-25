package io.github.greyp9.arwo.core.data.persist;

import io.github.greyp9.arwo.core.hash.text.FingerPrint;
import io.github.greyp9.arwo.core.http.Http;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.value.Value;

import java.io.File;
import java.io.IOException;
import java.util.logging.Logger;

public final class DataPersist {
    private final Logger logger = Logger.getLogger(getClass().getName());
    private final File folder;
    private final String extension;

    public DataPersist(File folder, String extension) {
        this.folder = folder;
        this.extension = extension;
    }

    public void run(final String label, final byte[] data) throws IOException {
        final String hash = FingerPrint.toHex256(data);
        final File file = new File(folder, Value.join(Http.Token.DOT, label, hash, extension));
        StreamU.write(file, data);
        logger.finest(Value.join(Http.Token.EQUALS, label, hash));
    }
}
