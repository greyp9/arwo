package io.github.greyp9.arwo.core.vm.mutex;

import java.util.Map;

@SuppressWarnings("SynchronizationOnLocalVariableOrMethodParameter")
public final class MapU {

    private MapU() {
    }

    public static Object get(final Map<?, ?> map, final Object key) {
        synchronized (map) {
            return map.get(key);
        }
    }

    public static <K, V> V put(final Map<K, V> map, final K key, final V value) {
        V valueOut;
        synchronized (map) {
            if (value == null) {
                valueOut = map.remove(key);
            } else {
                valueOut = map.put(key, value);
            }
        }
        return valueOut;
    }
}
