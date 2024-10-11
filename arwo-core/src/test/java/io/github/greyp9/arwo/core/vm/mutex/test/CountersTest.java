package io.github.greyp9.arwo.core.vm.mutex.test;

import io.github.greyp9.arwo.core.vm.mutex.Counters;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class CountersTest {
    //final Logger logger = Logger.getLogger(getClass().getName());

    @Test
    public void testCountersAdd() {
        final Counters counters = new Counters();
        Assertions.assertEquals(0, counters.size());
        final Counters.Counter a = counters.add("A", 1);
        Assertions.assertEquals(1, counters.size());
        Assertions.assertEquals("A", a.getName());
        Assertions.assertEquals(1, a.getValue());
        final int a1 = counters.getValue("A");
        Assertions.assertEquals(1, a1);
    }

    @Test
    public void testCountersAddAdd() {
        final Counters counters = new Counters();
        Assertions.assertEquals(0, counters.size());
        counters.add("A", 1);
        counters.add("A", 1);
        Assertions.assertEquals(1, counters.size());
        final int a = counters.getValue("A");
        Assertions.assertEquals(2, a);
    }

    @Test
    public void testCountersAddAB() {
        final Counters counters = new Counters();
        Assertions.assertEquals(0, counters.size());
        counters.add("A", 1);
        counters.add("B", 1);
        Assertions.assertEquals(2, counters.size());
        final int a = counters.getValue("A");
        final int b = counters.getValue("B");
        Assertions.assertEquals(1, a);
        Assertions.assertEquals(1, b);
    }
}
