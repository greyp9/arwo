package io.github.greyp9.arwo.core.task.type.store;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.io.StreamU;

public class StoreRunnable implements Runnable {
    private final StoreTask task;

    public StoreRunnable(final StoreTask task) {
        this.task = task;
    }

    @Override
    public final void run() {
        for (MetaFile metaFile : task.getMetaFiles()) {
            final String key = task.getName();
            final String valueType = task.getValue();
            if (StoreTask.TYPE_BODY.equals(valueType)) {
                final String value = UTF8Codec.toString(StreamU.readSafe(metaFile.getBytes()));
                task.getSecureStore().setPropertyProtectSafe(key, value);
            }
        }
    }
}
