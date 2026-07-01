package io.github.greyp9.arwo.core.vm.mutex;

import java.util.Map;
import java.util.Optional;

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

    @SafeVarargs
    public static <K, V> Map<K, V> join(final Map<K, V> target, final Map<K, V>... sources) {
        for (Map<K, V> source : sources) {
            Optional.ofNullable(source).ifPresent(target::putAll);
        }
        return target;
    }
}
