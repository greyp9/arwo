package io.github.greyp9.arwo.core.task.type.store;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.envsec.store.SecureStore;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.io.StreamU;

import java.io.IOException;
import java.util.Date;
import java.util.logging.Logger;

public class StoreRunnable implements Runnable {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final StoreTask task;

    public StoreRunnable(final StoreTask task) {
        this.task = task;
    }

    @Override
    public final void run() {
        boolean success = true;
        task.setDateStart(new Date());
        for (MetaFile metaFile : task.getMetaFiles()) {
            final String valueType = task.getValue();
            if (StoreTask.TYPE_BODY.equals(valueType)) {
                success &= storeBody(metaFile);
            }
        }
        task.setDateFinish(new Date());
        task.setExitValue(success ? 0 : -1);
    }

    private boolean storeBody(final MetaFile metaFile) {
        boolean success = true;
        final SecureStore secureStore = task.getSecureStore();
        logger.info(String.format("COUNT:%d", secureStore.size()));
        final String key = task.getKey();
        try {
            final String value = UTF8Codec.toString(StreamU.read(metaFile.getBytes()));
            secureStore.setProperty(key, value);
        } catch (IOException e) {
            logger.warning(String.format("WRITE:%s%s", key, e.getMessage()));
            success = false;
        }
        logger.info(String.format("WRITE:%s, COUNT:%d", key, secureStore.size()));
        return success;
    }
}
