package io.github.greyp9.arwo.core.xed.suite.test.choice1.test;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.hash.CRCU;
import io.github.greyp9.arwo.core.http.HttpArguments;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xed.view.XedCursorView;
import io.github.greyp9.arwo.core.xed.view.text.CursorTextView;
import io.github.greyp9.arwo.core.xed.xsd.choice1.test.Choice1OpTest;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactory;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;
import junit.framework.TestCase;
import org.junit.Assert;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.xml.namespace.QName;
import java.net.URL;
import java.util.Collection;
import java.util.logging.Logger;

public class Choice1CursorTest extends TestCase {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Override
    public void setUp() throws Exception {
        super.setUp();
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));
    }

    public void testChoice() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(Choice1OpTest.Const.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("{urn:arwo:choice1}jobs");
        // generate document
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(qname);
        logger.finest("\n" + DocumentU.toString(document));
        final Xed xed = new Xed(document, xsdTypes);
        // validate
        final Collection<String> messages0 = xed.validate();
        Assert.assertEquals("[]", messages0.toString());
        // navigate
        final Element jobs = xed.getXPather().getElement("/ch1:jobs");
        // view at root
        final XedCursor cursorJobs = new XedNav(xed).find(jobs);
        Assert.assertEquals("/", cursorJobs.getURI());
        final String renderJobs = new CursorTextView(new XedCursorView(cursorJobs)).render();
        logger.finest("Jobs\n" + renderJobs);
        Assert.assertEquals("044ebd63", CRCU.crc32String(UTF8Codec.toBytes(renderJobs)));
        // view at JobType
        final XedCursor cursorJobType = new XedNav(xed).find("job", cursorJobs);
        Assert.assertEquals("/2d467/", cursorJobType.getURI());
        final String renderJobType = new CursorTextView(new XedCursorView(cursorJobType)).render();
        logger.finest("JobType\n" + renderJobType);
        Assert.assertEquals("314a83a3", CRCU.crc32String(UTF8Codec.toBytes(renderJobType)));
        // insert
        final NameTypeValues ntv1 = HttpArguments.toArguments(
                "jobName=job1&jobEnabled=true&taskA/taskB/taskC=taskA");
        final ValueInstance value1 = ValueInstance.create(cursorJobType.getTypeInstance(), ntv1);
        final Element job1 = xed.create(cursorJobType.getParent().getElement(), value1);
        Assert.assertNotNull(job1);
        logger.finest("\n" + DocumentU.toString(document));
        // validate
        final Collection<String> messages1 = xed.validate();
        Assert.assertEquals("[]", messages1.toString());
        // insert
        final NameTypeValues ntv2 = HttpArguments.toArguments(
                "jobName=job2&jobEnabled=false&taskA/taskB/taskC=taskB");
        final ValueInstance value2 = ValueInstance.create(cursorJobType.getTypeInstance(), ntv2);
        final Element job2 = xed.create(cursorJobType.getParent().getElement(), value2);
        Assert.assertNotNull(job2);
        logger.finest("\n" + DocumentU.toString(document));
        // insert
        final NameTypeValues ntv3 = HttpArguments.toArguments(
                "jobName=job3&jobEnabled=true&taskA/taskB/taskC=taskC");
        final ValueInstance value3 = ValueInstance.create(cursorJobType.getTypeInstance(), ntv3);
        final Element job3 = xed.create(cursorJobType.getParent().getElement(), value3);
        Assert.assertNotNull(job3);
        logger.finest("\n" + DocumentU.toString(document));
        // view at JobType2
        final String renderJobType2 = new CursorTextView(new XedCursorView(cursorJobType)).render();
        logger.finest("JobType\n" + renderJobType2);
        Assert.assertEquals("4b0d6155", CRCU.crc32String(UTF8Codec.toBytes(renderJobType2)));
        // view at Job
        final XedCursor cursorJob1 = new XedNav(xed).find(job1);
        Assert.assertEquals("/2d467/0ca19/", cursorJob1.getURI());
        final String renderJob1 = new CursorTextView(new XedCursorView(cursorJob1)).render();
        logger.finest("Job\n" + renderJob1);
        Assert.assertEquals("2a727cd7", CRCU.crc32String(UTF8Codec.toBytes(renderJob1)));
        // view at Job
        final XedCursor cursorJob2 = new XedNav(xed).find(job2);
        Assert.assertEquals("/2d467/7fa8f/", cursorJob2.getURI());
        final String renderJob2 = new CursorTextView(new XedCursorView(cursorJob2)).render();
        logger.finest("Job\n" + renderJob2);
        Assert.assertEquals("c28f6339", CRCU.crc32String(UTF8Codec.toBytes(renderJob2)));
        // view at Job
        final XedCursor cursorJob3 = new XedNav(xed).find(job3);
        Assert.assertEquals("/2d467/eab35/", cursorJob3.getURI());
        final String renderJob3 = new CursorTextView(new XedCursorView(cursorJob3)).render();
        logger.finest("Job\n" + renderJob3);
        Assert.assertEquals("3c3dd30f", CRCU.crc32String(UTF8Codec.toBytes(renderJob3)));
        // insert A
        final Element taskA = xed.getXPather().getElement("/ch1:jobs/ch1:job/ch1:taskA");
        final XedCursor cursorTaskA = new XedNav(xed).find(taskA);
        Assert.assertEquals("/2d467/0ca19/3a1a0/20111/d7ab5/", cursorTaskA.getURI());
    }

    public void testChoiceExisting() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(Choice1OpTest.Const.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final Document document = DocumentU.toDocument(StreamU.read(ResourceU.resolve(Const.XML)));
        logger.finest("\n" + DocumentU.toString(document));
        final Xed xed = new Xed(document, xsdTypes);
        // validate
        final Element taskA = xed.getXPather().getElement("/ch1:jobs/ch1:job/ch1:taskA");
        final XedCursor cursorTaskAByNode = new XedNav(xed).find(taskA);
        Assert.assertEquals("/2d467/0ca19/3a1a0/20111/d7ab5/", cursorTaskAByNode.getURI());
        final XedCursor cursorTaskAByPath = new XedNav(xed).find("/2d467/0ca19/3a1a0/20111/d7ab5/");
        Assert.assertEquals(cursorTaskAByNode.getNode(), cursorTaskAByPath.getNode());
        // insert A
        final NameTypeValues ntvA = HttpArguments.toArguments("attributeA=valueA&taskName=nameA&taskEnabled=true");
        final ValueInstance valueA = ValueInstance.create(cursorTaskAByNode.getTypeInstance(), ntvA);
        final Element taskA2 = xed.update(cursorTaskAByNode.getElement(), valueA);
        Assert.assertNotNull(taskA2);
        // validate
        final Element taskB = xed.getXPather().getElement("/ch1:jobs/ch1:job/ch1:taskB");
        final XedCursor cursorTaskBByNode = new XedNav(xed).find(taskB);
        Assert.assertEquals("/2d467/7fa8f/3a1a0/06db8/9ffaf/", cursorTaskBByNode.getURI());
        final XedCursor cursorTaskBByPath = new XedNav(xed).find("/2d467/7fa8f/3a1a0/06db8/9ffaf/");
        Assert.assertEquals(cursorTaskBByNode.getNode(), cursorTaskBByPath.getNode());
        // insert B
        final NameTypeValues ntvB = HttpArguments.toArguments("attributeB=valueB&taskName=nameB&taskEnabled=false");
        final ValueInstance valueB = ValueInstance.create(cursorTaskBByNode.getTypeInstance(), ntvB);
        final Element taskB2 = xed.update(cursorTaskBByNode.getElement(), valueB);
        Assert.assertNotNull(taskB2);
        // validate
        logger.finest("\n" + DocumentU.toString(document));
    }

    public void testChoiceUpdateExisting() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(Choice1OpTest.Const.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final Document document = DocumentU.toDocument(StreamU.read(ResourceU.resolve(Const.XML)));
        logger.finest("\n" + DocumentU.toString(document));
        final Xed xed = new Xed(document, xsdTypes);
        // validate
        final XedCursor cursorJob1 = new XedNav(xed).find("/2d467/0ca19/");
        // update job to type B
        final NameTypeValues ntvA2B = HttpArguments.toArguments("taskA/taskB/taskC=taskB");
        final ValueInstance valueA2B = ValueInstance.create(cursorJob1.getTypeInstance(), ntvA2B);
        final Element job1A2B = xed.update(cursorJob1.getElement(), valueA2B);
        Assert.assertNotNull(job1A2B);
        // update new B task
        final XedCursor cursorJob1Task = new XedNav(xed).find("/2d467/0ca19/3a1a0/06db8/9ffaf/");
        final NameTypeValues ntvB = HttpArguments.toArguments("attributeB=valueB&taskName=nameB&taskEnabled=false");
        final ValueInstance valueB = ValueInstance.create(cursorJob1Task.getTypeInstance(), ntvB);
        final Element task1 = xed.update(cursorJob1Task.getElement(), valueB);
        Assert.assertNotNull(task1);
        // view at Job
        final String renderJob1 = new CursorTextView(new XedCursorView(cursorJob1)).render();
        logger.finest("Job\n" + renderJob1);
        Assert.assertEquals("538548eb", CRCU.crc32String(UTF8Codec.toBytes(renderJob1)));
        // validate
        logger.finest("\n" + DocumentU.toString(document));
    }

    public static class Const {
        public static final String XML = "io/github/greyp9/arwo/xsd/choice1/choice1-A.xml";
    }
}
