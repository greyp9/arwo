package io.github.greyp9.arwo.core.xed.xsd.choice1.test;

import io.github.greyp9.arwo.core.app.test.TestApp;
import io.github.greyp9.arwo.core.hash.CRCU;
import io.github.greyp9.arwo.core.http.HttpArguments;
import io.github.greyp9.arwo.core.lang.SystemU;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.value.NameTypeValues;
import io.github.greyp9.arwo.core.xed.cursor.XedCursor;
import io.github.greyp9.arwo.core.xed.model.Xed;
import io.github.greyp9.arwo.core.xed.nav.XedNav;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xsd.document.DocumentFactory;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import io.github.greyp9.arwo.core.xsd.value.ValueInstance;
import org.junit.Assert;
import org.junit.Assume;
import org.junit.Before;
import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.xml.namespace.QName;
import java.net.URL;
import java.util.Collection;
import java.util.logging.Logger;

@SuppressWarnings("checkstyle:magicnumber")
public class Choice1OpTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Before
    public void setUp() throws Exception {
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));
    }

    @Test
    public void testNavigate() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(TestApp.Resources.XSD_CHOICE1);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("{urn:arwo:choice1}jobs");
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(qname);
        final Xed xed = new Xed(document, xsdTypes);
        logger.finest(DocumentU.toString(xed.getDocument()));
        // navigate
        final Element jobs = xed.getXPather().getElement("/ch1:jobs");
        final XedCursor cursorJobsByNode = new XedNav(xed).find(jobs);
        Assert.assertEquals("/", cursorJobsByNode.getURI());
        final XedCursor cursorJobsByPath = new XedNav(xed).find("/");
        Assert.assertEquals(cursorJobsByNode.getElement(), cursorJobsByPath.getElement());
        // navigate
        XedCursor cursorJobTIByNode = new XedNav(xed).find("job", cursorJobsByNode);
        Assert.assertEquals("/2d467/", cursorJobTIByNode.getURI());
        XedCursor cursorEJobTIByNode = new XedNav(xed).find("enhancedJob", cursorJobsByNode);
        Assert.assertEquals("/d1be5/", cursorEJobTIByNode.getURI());
    }

    @Test
    public void testCreateJob() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(TestApp.Resources.XSD_CHOICE1);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("{urn:arwo:choice1}jobs");
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(qname);
        final Xed xed = new Xed(document, xsdTypes);
        logger.finest(DocumentU.toString(xed.getDocument()));
        // navigate
        final Element jobs = xed.getXPather().getElement("/ch1:jobs");
        final XedCursor cursorJobsByNode = new XedNav(xed).find(jobs);
        XedCursor cursorJobType = new XedNav(xed).find("job", cursorJobsByNode);
        // insert
        final NameTypeValues ntv1 = HttpArguments.toArguments(
                "jobName=job1&jobEnabled=true&taskA/taskB/taskC=taskA");
        final ValueInstance value1 = ValueInstance.create(cursorJobType.getTypeInstance(), ntv1);
        final Element job1 = xed.create(cursorJobType.getParent().getElement(), value1);
        Assert.assertNotNull(job1);
        final Collection<String> messages1 = xed.validate();
        Assert.assertEquals("[]", messages1.toString());
        // insert
        final NameTypeValues ntv2 = HttpArguments.toArguments(
                "jobName=job2&jobEnabled=true&taskA/taskB/taskC=taskB");
        final ValueInstance value2 = ValueInstance.create(cursorJobType.getTypeInstance(), ntv2);
        final Element job2 = xed.create(cursorJobType.getParent().getElement(), value2);
        Assert.assertNotNull(job2);
        final Collection<String> messages2 = xed.validate();
        Assert.assertEquals("[]", messages2.toString());
        // validate
        logger.finest(DocumentU.toString(document));
        final byte[] xml = DocumentU.toXml(document);
        Assume.assumeTrue(SystemU.javaVersion().startsWith("1.8"));
        Assert.assertEquals(297, xml.length);
        Assert.assertEquals("f71d78bc", CRCU.crc32String(xml));
    }

    @Test
    public void testCreateUpdateJob() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(TestApp.Resources.XSD_CHOICE1);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("{urn:arwo:choice1}jobs");
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(qname);
        final Xed xed = new Xed(document, xsdTypes);
        logger.finest(DocumentU.toString(xed.getDocument()));
        // navigate
        final Element jobs = xed.getXPather().getElement("/ch1:jobs");
        final XedCursor cursorJobsByNode = new XedNav(xed).find(jobs);
        XedCursor cursorJobType = new XedNav(xed).find("job", cursorJobsByNode);
        // insert
        final NameTypeValues ntv1 = HttpArguments.toArguments(
                "jobName=job1&jobEnabled=true&taskA/taskB/taskC=taskA");
        final ValueInstance value1 = ValueInstance.create(cursorJobType.getTypeInstance(), ntv1);
        final Element job1 = xed.create(cursorJobType.getParent().getElement(), value1);
        Assert.assertNotNull(job1);
        final Collection<String> messages1 = xed.validate();
        Assert.assertEquals("[]", messages1.toString());
        // update
        final XedCursor cursor1 = new XedNav(xed).find(job1);
        final NameTypeValues ntv2 = HttpArguments.toArguments(
                "jobName=job1&jobEnabled=true&taskA/taskB/taskC=taskB");
        final ValueInstance value2 = ValueInstance.create(cursorJobType.getTypeInstance(), ntv2);
        final Element job2 = xed.update(cursor1.getElement(), value2);
        Assert.assertNotNull(job2);
        // validate
        logger.finest(DocumentU.toString(document));
        final byte[] xml = DocumentU.toXml(document);
        Assume.assumeTrue(SystemU.javaVersion().startsWith("1.8"));
        Assert.assertEquals(196, xml.length);
        Assert.assertEquals("a44539b7", CRCU.crc32String(xml));
    }

    @Test
    public void testCreateEnhancedJob() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(TestApp.Resources.XSD_CHOICE1);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("{urn:arwo:choice1}jobs");
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(qname);
        final Xed xed = new Xed(document, xsdTypes);
        logger.finest(DocumentU.toString(xed.getDocument()));
        // navigate
        final Element jobs = xed.getXPather().getElement("/ch1:jobs");
        final XedCursor cursorJobsByNode = new XedNav(xed).find(jobs);
        XedCursor cursorJobType = new XedNav(xed).find("enhancedJob", cursorJobsByNode);
        // insert
        final NameTypeValues ntv1 = HttpArguments.toArguments(
                "jobName=job1&jobEnabled=true&before=before1&after=after1&taskA/taskB/taskC=taskA");
        final ValueInstance value1 = ValueInstance.create(cursorJobType.getTypeInstance(), ntv1);
        final Element job1 = xed.create(cursorJobType.getParent().getElement(), value1);
        Assert.assertNotNull(job1);
        final Collection<String> messages1 = xed.validate();
        Assert.assertEquals("[]", messages1.toString());
        // insert
        final NameTypeValues ntv2 = HttpArguments.toArguments(
                "jobName=job2&jobEnabled=true&before=before2&after=after2&taskA/taskB/taskC=taskB");
        final ValueInstance value2 = ValueInstance.create(cursorJobType.getTypeInstance(), ntv2);
        final Element job2 = xed.create(cursorJobType.getParent().getElement(), value2);
        Assert.assertNotNull(job2);
        final Collection<String> messages2 = xed.validate();
        Assert.assertEquals("[]", messages2.toString());
        // validate
        logger.finest(DocumentU.toString(document));
        final byte[] xml = DocumentU.toXml(document);
        Assume.assumeTrue(SystemU.javaVersion().startsWith("1.8"));
        Assert.assertEquals(423, xml.length);
        Assert.assertEquals("59d3c6b1", CRCU.crc32String(xml));
    }

    @Test
    public void testCreateUpdateEnhancedJob() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(TestApp.Resources.XSD_CHOICE1);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("{urn:arwo:choice1}jobs");
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(qname);
        final Xed xed = new Xed(document, xsdTypes);
        logger.finest(DocumentU.toString(xed.getDocument()));
        // navigate
        final Element jobs = xed.getXPather().getElement("/ch1:jobs");
        final XedCursor cursorJobsByNode = new XedNav(xed).find(jobs);
        XedCursor cursorJobType = new XedNav(xed).find("enhancedJob", cursorJobsByNode);
        // insert
        final NameTypeValues ntv1 = HttpArguments.toArguments(
                "jobName=job1&jobEnabled=true&before=before1&after=after1&taskA/taskB/taskC=taskA");
        final ValueInstance value1 = ValueInstance.create(cursorJobType.getTypeInstance(), ntv1);
        final Element job1 = xed.create(cursorJobType.getParent().getElement(), value1);
        Assert.assertNotNull(job1);
        final Collection<String> messages1 = xed.validate();
        Assert.assertEquals("[]", messages1.toString());
        // update
        final XedCursor cursor1 = new XedNav(xed).find(job1);
        final NameTypeValues ntv2 = HttpArguments.toArguments(
                "jobName=job2&jobEnabled=true&before=before2&after=after2&taskA/taskB/taskC=taskB");
        final ValueInstance value2 = ValueInstance.create(cursorJobType.getTypeInstance(), ntv2);
        final Element job2 = xed.update(cursor1.getElement(), value2);
        Assert.assertNotNull(job2);
        final Collection<String> messages2 = xed.validate();
        Assert.assertEquals("[]", messages2.toString());
        // validate
        logger.finest(DocumentU.toString(document));
        final byte[] xml = DocumentU.toXml(document);
        Assume.assumeTrue(SystemU.javaVersion().startsWith("1.8"));
        Assert.assertEquals(259, xml.length);
        Assert.assertEquals("5720f73f", CRCU.crc32String(xml));
    }
}
