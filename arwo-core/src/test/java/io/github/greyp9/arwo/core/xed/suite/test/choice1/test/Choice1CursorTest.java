package io.github.greyp9.arwo.core.xed.suite.test.choice1.test;

import io.github.greyp9.arwo.core.app.test.TestApp;
import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.hash.CRCU;
import io.github.greyp9.arwo.core.http.HttpArguments;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xed.bundle.XsdBundle;
import io.github.greyp9.arwo.core.xed.bundle.XsdBundles;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.view.XedCursorView;
import io.github.greyp9.arwo.core.xed.view.text.CursorTextView;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactory;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.xml.namespace.QName;
import java.net.URL;
import java.util.Collection;
import java.util.Locale;
import java.util.logging.Logger;

public class Choice1CursorTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @BeforeEach
    public void setUp() throws Exception {
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));
    }

    @Test
    public void testChoice() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(TestApp.Resources.XSD_CHOICE1);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("{urn:arwo:choice1}jobs");
        // generate document
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(qname);
        logger.finest("\n" + DocumentU.toString(document));
        final Xed xed = new Xed(document, xsdTypes);
        final XsdBundle xsdBundle = new XsdBundle(new XsdBundles(xsdTypes, Locale.getDefault()));
        final Xed xedUI = new Xed(xed.getDocument(), xed.getXsdTypes(), xsdBundle);
        // validate
        final Collection<String> messages0 = xed.validate();
        Assertions.assertEquals("[]", messages0.toString());
        // navigate
        final Element jobs = xed.getXPather().getElement("/ch1:jobs");
        // view at root
        final XedCursor cursorJobs = new XedNav(xedUI).find(jobs);
        Assertions.assertEquals("/", cursorJobs.getURI());
        final String renderJobs = new CursorTextView(new XedCursorView(cursorJobs)).render();
        logger.finest("Jobs\n" + renderJobs);
        Assertions.assertEquals("044ebd63", CRCU.crc32String(UTF8Codec.toBytes(renderJobs)));
        // view at JobType
        final XedCursor cursorJobType = new XedNav(xedUI).find("job", cursorJobs);
        Assertions.assertEquals("/2d467/", cursorJobType.getURI());
        final String renderJobType = new CursorTextView(new XedCursorView(cursorJobType)).render();
        logger.finest("JobType\n" + renderJobType);
        Assertions.assertEquals("d5331ee5", CRCU.crc32String(UTF8Codec.toBytes(renderJobType)));
        // insert
        final NameTypeValues ntv1 = HttpArguments.toArguments(
                "jobName=job1&jobEnabled=true&taskA/taskB/taskC=taskA");
        final ValueInstance value1 = ValueInstance.create(cursorJobType.getTypeInstance(), ntv1);
        final Element job1 = xedUI.create(cursorJobType.getParent().getElement(), value1);
        Assertions.assertNotNull(job1);
        logger.finest("\n" + DocumentU.toString(document));
        // validate
        final Collection<String> messages1 = xedUI.validate();
        Assertions.assertEquals("[]", messages1.toString());
        // insert
        final NameTypeValues ntv2 = HttpArguments.toArguments(
                "jobName=job2&jobEnabled=false&taskA/taskB/taskC=taskB");
        final ValueInstance value2 = ValueInstance.create(cursorJobType.getTypeInstance(), ntv2);
        final Element job2 = xedUI.create(cursorJobType.getParent().getElement(), value2);
        Assertions.assertNotNull(job2);
        logger.finest("\n" + DocumentU.toString(document));
        // insert
        final NameTypeValues ntv3 = HttpArguments.toArguments(
                "jobName=job3&jobEnabled=true&taskA/taskB/taskC=taskC");
        final ValueInstance value3 = ValueInstance.create(cursorJobType.getTypeInstance(), ntv3);
        final Element job3 = xedUI.create(cursorJobType.getParent().getElement(), value3);
        Assertions.assertNotNull(job3);
        logger.finest("\n" + DocumentU.toString(document));
        // view at JobType2
        final String renderJobType2 = new CursorTextView(new XedCursorView(cursorJobType)).render();
        logger.finest("JobType\n" + renderJobType2);
        Assertions.assertEquals("ecb2cf83", CRCU.crc32String(UTF8Codec.toBytes(renderJobType2)));
        // view at Job
        final XedCursor cursorJob1 = new XedNav(xedUI).find(job1);
        Assertions.assertEquals("/2d467/0ca19/", cursorJob1.getURI());
        final String renderJob1 = new CursorTextView(new XedCursorView(cursorJob1)).render();
        logger.finest("Job\n" + renderJob1);
        Assertions.assertEquals("2a727cd7", CRCU.crc32String(UTF8Codec.toBytes(renderJob1)));
        // view at Job
        final XedCursor cursorJob2 = new XedNav(xedUI).find(job2);
        Assertions.assertEquals("/2d467/7fa8f/", cursorJob2.getURI());
        final String renderJob2 = new CursorTextView(new XedCursorView(cursorJob2)).render();
        logger.finest("Job\n" + renderJob2);
        Assertions.assertEquals("c28f6339", CRCU.crc32String(UTF8Codec.toBytes(renderJob2)));
        // view at Job
        final XedCursor cursorJob3 = new XedNav(xedUI).find(job3);
        Assertions.assertEquals("/2d467/eab35/", cursorJob3.getURI());
        final String renderJob3 = new CursorTextView(new XedCursorView(cursorJob3)).render();
        logger.finest("Job\n" + renderJob3);
        Assertions.assertEquals("3c3dd30f", CRCU.crc32String(UTF8Codec.toBytes(renderJob3)));
        // insert A
        final Element taskA = xedUI.getXPather().getElement("/ch1:jobs/ch1:job/ch1:taskA");
        final XedCursor cursorTaskA = new XedNav(xedUI).find(taskA);
        Assertions.assertEquals("/2d467/0ca19/3a1a0/20111/d7ab5/", cursorTaskA.getURI());
    }

    @Test
    public void testChoiceExisting() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(TestApp.Resources.XSD_CHOICE1);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final Document document = DocumentU.toDocument(StreamU.read(ResourceU.resolve(Const.XML)));
        logger.finest("\n" + DocumentU.toString(document));
        final Xed xed = new Xed(document, xsdTypes);
        // validate
        final Element taskA = xed.getXPather().getElement("/ch1:jobs/ch1:job/ch1:taskA");
        final XedCursor cursorTaskAByNode = new XedNav(xed).find(taskA);
        Assertions.assertEquals("/2d467/0ca19/3a1a0/20111/d7ab5/", cursorTaskAByNode.getURI());
        final XedCursor cursorTaskAByPath = new XedNav(xed).find("/2d467/0ca19/3a1a0/20111/d7ab5/");
        Assertions.assertEquals(cursorTaskAByNode.getNode(), cursorTaskAByPath.getNode());
        // insert A
        final NameTypeValues ntvA = HttpArguments.toArguments("attributeA=valueA&taskName=nameA&taskEnabled=true");
        final ValueInstance valueA = ValueInstance.create(cursorTaskAByNode.getTypeInstance(), ntvA);
        final Element taskA2 = xed.update(cursorTaskAByNode.getElement(), valueA);
        Assertions.assertNotNull(taskA2);
        // validate
        final Element taskB = xed.getXPather().getElement("/ch1:jobs/ch1:job/ch1:taskB");
        final XedCursor cursorTaskBByNode = new XedNav(xed).find(taskB);
        Assertions.assertEquals("/2d467/7fa8f/3a1a0/06db8/9ffaf/", cursorTaskBByNode.getURI());
        final XedCursor cursorTaskBByPath = new XedNav(xed).find("/2d467/7fa8f/3a1a0/06db8/9ffaf/");
        Assertions.assertEquals(cursorTaskBByNode.getNode(), cursorTaskBByPath.getNode());
        // insert B
        final NameTypeValues ntvB = HttpArguments.toArguments("attributeB=valueB&taskName=nameB&taskEnabled=false");
        final ValueInstance valueB = ValueInstance.create(cursorTaskBByNode.getTypeInstance(), ntvB);
        final Element taskB2 = xed.update(cursorTaskBByNode.getElement(), valueB);
        Assertions.assertNotNull(taskB2);
        // validate
        logger.finest("\n" + DocumentU.toString(document));
    }

    @Test
    public void testChoiceUpdateExisting() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(TestApp.Resources.XSD_CHOICE1);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final Document document = DocumentU.toDocument(StreamU.read(ResourceU.resolve(Const.XML)));
        logger.finest("\n" + DocumentU.toString(document));
        final Xed xed = new Xed(document, xsdTypes);
        final XsdBundle xsdBundle = new XsdBundle(new XsdBundles(xsdTypes, Locale.getDefault()));
        final Xed xedUI = new Xed(xed.getDocument(), xed.getXsdTypes(), xsdBundle);
        // validate
        final XedCursor cursorJob1 = new XedNav(xedUI).find("/2d467/0ca19/");
        // update job to type B
        final NameTypeValues ntvA2B = HttpArguments.toArguments("taskA/taskB/taskC=taskB&jobEnabled=true");
        final ValueInstance valueA2B = ValueInstance.create(cursorJob1.getTypeInstance(), ntvA2B);
        final Element job1A2B = xedUI.update(cursorJob1.getElement(), valueA2B);
        Assertions.assertNotNull(job1A2B);
        // update new B task
        final XedCursor cursorJob1Task = new XedNav(xedUI).find("/2d467/0ca19/3a1a0/06db8/9ffaf/");
        final NameTypeValues ntvB = HttpArguments.toArguments("attributeB=valueB&taskName=nameB&taskEnabled=false");
        final ValueInstance valueB = ValueInstance.create(cursorJob1Task.getTypeInstance(), ntvB);
        final Element task1 = xedUI.update(cursorJob1Task.getElement(), valueB);
        Assertions.assertNotNull(task1);
        // view at Job
        final String renderJob1 = new CursorTextView(new XedCursorView(cursorJob1)).render();
        logger.finest("Job\n" + renderJob1);
        Assertions.assertEquals("538548eb", CRCU.crc32String(UTF8Codec.toBytes(renderJob1)));
        // validate
        logger.finest("\n" + DocumentU.toString(document));
    }

    public static class Const {
        public static final String XML = "io/github/greyp9/arwo/xsd/choice1/choice1-A.xml";
    }
}
