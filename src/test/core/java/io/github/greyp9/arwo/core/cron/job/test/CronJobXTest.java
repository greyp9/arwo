package io.github.greyp9.arwo.core.cron.job.test;

import io.github.greyp9.arwo.core.cron.job.CronJob;
import io.github.greyp9.arwo.core.cron.job.CronJobX;
import io.github.greyp9.arwo.core.cron.job.factory.CronJobFactory;
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
            final String cron = element.getAttribute("cron");
            final String name = element.getAttribute("name");
            final boolean enabled = Boolean.parseBoolean(element.getAttribute("enabled"));
            final Element child = ElementU.getChildren(element).iterator().next();
            Assert.assertNotNull(cron);
            Assert.assertNotNull(name);
            Assert.assertNotNull(enabled);
            Assert.assertNotNull(child);
            final String type = child.getLocalName();
            final CronJob cronJob = new CronJobFactory().create(Value.join(" ", cron, type));
            final CronJobX cronJobX = new CronJobX(name, enabled, cronJob, child);
            Assert.assertTrue(cronJobX.getName().startsWith("type"));
            Assert.assertTrue(cronJobX.isEnabled());
            Assert.assertTrue(cronJobX.getElement().getLocalName().startsWith("type"));
        }
    }

    private static class Const {
        private static final String XML = "io/github/greyp9/arwo/cron/tab1/cronTab1.xml";
    }
}
