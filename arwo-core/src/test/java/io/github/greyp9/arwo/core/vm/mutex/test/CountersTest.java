package io.github.greyp9.arwo.core.vm.mutex.test;

import io.github.greyp9.arwo.core.vm.mutex.CounterInsert;
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

    @Test
    @SuppressWarnings("checkstyle:magicnumber")
    public void testCountersComplex() {
        final Counters counters = new Counters();
        counters.add("item1", new CounterInsert(1, 10));
        counters.add("item1", new CounterInsert(1, 20));
        counters.add("item1", new CounterInsert(1, 30));
        counters.add("item2", new CounterInsert(2, 25));
        counters.add("item2", new CounterInsert(2, 15));
        counters.add("item2", new CounterInsert(2, 5));

        final int[] values1 = counters.getValues("item1");
        Assertions.assertNotNull(values1);
        Assertions.assertEquals(4, values1.length);
        Assertions.assertEquals(3, values1[0]);
        Assertions.assertEquals(60, values1[1]);
        Assertions.assertEquals(10, values1[2]);
        Assertions.assertEquals(30, values1[3]);

        final int[] values2 = counters.getValues("item2");
        Assertions.assertNotNull(values2);
        Assertions.assertEquals(4, values2.length);
        Assertions.assertEquals(6, values2[0]);
        Assertions.assertEquals(45, values2[1]);
        Assertions.assertEquals(5, values2[2]);
        Assertions.assertEquals(25, values2[3]);
    }
}
