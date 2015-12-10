package io.github.greyp9.arwo.core.vm.mutex;

import java.util.Collection;
import java.util.Iterator;

@SuppressWarnings("SynchronizationOnLocalVariableOrMethodParameter")
public final class CollectionU {

    private CollectionU() {
    }

    public static <T> boolean add(final Collection<T> collection, final T object) {
        synchronized (collection) {
            return collection.add(object);
        }
    }

    public static <T> boolean remove(final Collection<T> collection, final T object) {
        synchronized (collection) {
            return collection.remove(object);
        }
    }

    public static <T> T removeNext(final Collection<T> collection) {
        T removed = null;
        synchronized (collection) {
            final Iterator<T> iterator = collection.iterator();
            if (iterator.hasNext()) {
                final T object = iterator.next();
                remove(collection, object);
                removed = object;
            }
        }
        return removed;
    }

    public static <T> Collection<T> copy(final Collection<T> collectionTo, final Collection<T> collectionFrom) {
        synchronized (collectionFrom) {
            collectionTo.addAll(collectionFrom);
        }
        return collectionTo;
    }

    public static <T> void clear(final Collection<T> collection) {
        synchronized (collection) {
            collection.clear();
        }
    }
}
