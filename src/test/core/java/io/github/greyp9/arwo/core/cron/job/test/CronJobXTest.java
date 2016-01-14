package io.github.greyp9.arwo.core.cron.job.test;

import io.github.greyp9.arwo.core.cron.job.CronJob;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xpath.XPather;
import junit.framework.TestCase;
import org.junit.Assert;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.util.List;

public class CronJobXTest extends TestCase {

    public void testJobX() throws Exception {
        final Document document = DocumentU.toDocument(StreamU.read(ResourceU.resolve(Const.XML)));
        final List<Element> elements = new XPather(document).getElements("/cronTab/cronJob");
        for (Element element : elements) {
            final String cron = ElementU.getAttribute(element, "cron");
            final String name = ElementU.getAttribute(element, "name");
            final boolean enabled = Boolean.parseBoolean(ElementU.getAttribute(element, "enabled"));
            final Element child = ElementU.getChildren(element).iterator().next();
            Assert.assertNotNull(cron);
            Assert.assertNotNull(name);
            Assert.assertNotNull(enabled);
            Assert.assertNotNull(child);
            final String type = child.getLocalName();
            final CronJob cronJob = new CronJob(name, enabled, Value.join(" ", cron, type), child);
            Assert.assertTrue(cronJob.getName().startsWith("type"));
            Assert.assertTrue(cronJob.isEnabled());
            Assert.assertTrue(cronJob.getElement().getLocalName().startsWith("type"));
        }
    }

    private static class Const {
        private static final String XML = "io/github/greyp9/arwo/cron/tab1/cronTab1.xml";
    }
}
