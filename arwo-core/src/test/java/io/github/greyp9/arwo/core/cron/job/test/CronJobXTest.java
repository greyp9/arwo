package io.github.greyp9.arwo.core.cron.job.test;

import io.github.greyp9.arwo.core.cron.job.CronJob;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.ElementU;
import io.github.greyp9.arwo.core.xpath.XPather;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.util.List;

public class CronJobXTest {

    @Test
    public void testJobX() throws Exception {
        final Document document = DocumentU.toDocument(StreamU.read(ResourceU.resolve(Const.XML)));
        final List<Element> elements = new XPather(document).getElements("/cronTab/cronJob");
        for (Element element : elements) {
            final String cron = ElementU.getAttribute(element, "cron");
            final String name = ElementU.getAttribute(element, "name");
            final boolean enabled = Boolean.parseBoolean(ElementU.getAttribute(element, "enabled"));
            final Element child = ElementU.getChildren(element).iterator().next();
            Assertions.assertNotNull(cron);
            Assertions.assertNotNull(name);
            Assertions.assertNotNull(enabled);
            Assertions.assertNotNull(child);
            final String type = child.getLocalName();
            final CronJob cronJob = new CronJob(name, enabled, Value.join(" ", cron, type), child);
            Assertions.assertTrue(cronJob.getName().startsWith("type"));
            Assertions.assertTrue(cronJob.isEnabled());
            Assertions.assertTrue(cronJob.getElement().getLocalName().startsWith("type"));
        }
    }

    private static class Const {
        private static final String XML = "io/github/greyp9/arwo/cron/tab1/cronTab1.xml";
    }
}
