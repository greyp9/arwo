package io.github.greyp9.arwo.core.jce.test;

import org.junit.jupiter.api.Test;

import java.security.Provider;
import java.security.Security;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.logging.Logger;

public class EnumerateTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testEnumerateAlgorithms() {
        final Provider[] providers = Security.getProviders();
        for (Provider provider : providers) {
            final Set<Map.Entry<Object, Object>> entries = provider.entrySet();
            for (Map.Entry<Object, Object> entry : entries) {
                final String value = String.format("PROVIDER:[%s] KEY:[%s] VALUE:[%s]",
                        provider.getName(), entry.getKey(), entry.getValue());
                Optional.of(value)
                        .filter(v -> v.contains("KeyGenerator"))
                        .filter(v -> v.contains("AES"))
                        .ifPresent(match -> logger.finest(value));
            }
        }
    }
}
