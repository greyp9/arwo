package io.github.greyp9.arwo.core.task.type.store;

import io.github.greyp9.arwo.core.envsec.store.SecureStore;
import io.github.greyp9.arwo.core.file.meta.MetaFile;
import io.github.greyp9.arwo.core.task.core.Task;

import java.io.File;
import java.util.Date;
import java.util.List;

public class StoreTask extends Task {
    private final String key;
    private final String value;
    private final SecureStore secureStore;
    private final List<MetaFile> metaFiles;

    public StoreTask(final String name, final String key, final String value,
                     final SecureStore secureStore, final List<MetaFile> metaFiles) {
        super(name, new Date());
        this.key = key;
        this.value = value;
        this.secureStore = secureStore;
        this.metaFiles = metaFiles;
    }

    public final String getKey() {
        return key;
    }

    public final String getValue() {
        return value;
    }

    public final SecureStore getSecureStore() {
        return secureStore;
    }

    public final List<MetaFile> getMetaFiles() {
        return metaFiles;
    }

    @Override
    public final Runnable createRunnable(final File ignored) {
        return new StoreRunnable(this);
    }

    public static final String TYPE_BODY = "$BODY";
}
