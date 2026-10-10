package io.github.greyp9.arwo.app.core.lookup;

import io.github.greyp9.arwo.core.envsec.store.SecureStore;
import io.github.greyp9.arwo.core.naming.AppNaming;
import io.github.greyp9.arwo.core.value.Value;

public final class AppLookup {

    private AppLookup() {
    }

    public static SecureStore secureStore() {
        final Class<SecureStore> c = SecureStore.class;
        return Value.as(AppNaming.lookup(c.getName(), c.getName()), c);
    }
}
