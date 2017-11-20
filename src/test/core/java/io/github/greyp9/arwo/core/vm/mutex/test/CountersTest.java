package io.github.greyp9.arwo.core.vm.mutex.test;

import io.github.greyp9.arwo.core.vm.mutex.Counters;
import junit.framework.TestCase;
import org.junit.Assert;

public class CountersTest extends TestCase {
    //final Logger logger = Logger.getLogger(getClass().getName());

    public void testCountersAdd() throws Exception {
        final Counters counters = new Counters();
        Assert.assertEquals(0, counters.size());
        final Counters.Counter a = counters.add("A", 1);
        Assert.assertEquals(1, counters.size());
        Assert.assertEquals("A", a.getName());
        Assert.assertEquals(1, a.getValue());
        final int a1 = counters.getValue("A");
        Assert.assertEquals(1, a1);
    }

    public void testCountersAddAdd() throws Exception {
        final Counters counters = new Counters();
        Assert.assertEquals(0, counters.size());
        counters.add("A", 1);
        counters.add("A", 1);
        Assert.assertEquals(1, counters.size());
        final int a = counters.getValue("A");
        Assert.assertEquals(2, a);
    }

    public void testCountersAddAB() throws Exception {
        final Counters counters = new Counters();
        Assert.assertEquals(0, counters.size());
        counters.add("A", 1);
        counters.add("B", 1);
        Assert.assertEquals(2, counters.size());
        final int a = counters.getValue("A");
        final int b = counters.getValue("B");
        Assert.assertEquals(1, a);
        Assert.assertEquals(1, b);
    }
}
