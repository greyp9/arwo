package io.github.greyp9.arwo.core.xed.xsd.choiceNoNs.test;

import io.github.greyp9.arwo.core.hash.CRCU;
import io.github.greyp9.arwo.core.http.HttpArguments;
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
import junit.framework.TestCase;
import org.junit.Assert;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.xml.namespace.QName;
import java.net.URL;
import java.util.Collection;
import java.util.logging.Logger;

public class ChoiceNoNsOpTest extends TestCase {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Override
    public void setUp() throws Exception {
        super.setUp();
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));
    }

    public void testNavigate() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(Const.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("jobs");
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(qname);
        final Xed xed = new Xed(document, xsdTypes);
        logger.finest(DocumentU.toString(xed.getDocument()));
        // navigate
        final Element jobs = xed.getXPather().getElement("/jobs");
        final XedCursor cursorJobsByNode = new XedNav(xed).find(jobs);
        Assert.assertEquals("/", cursorJobsByNode.getURI());
        final XedCursor cursorJobsByPath = new XedNav(xed).find("/");
        Assert.assertEquals(cursorJobsByNode.getElement(), cursorJobsByPath.getElement());
        // navigate
        XedCursor cursorJobTIByNode = new XedNav(xed).find("job", cursorJobsByNode);
        Assert.assertEquals("/36a9a/", cursorJobTIByNode.getURI());
        XedCursor cursorEJobTIByNode = new XedNav(xed).find("enhancedJob", cursorJobsByNode);
        Assert.assertEquals("/92da2/", cursorEJobTIByNode.getURI());
    }

    public void testCreateJob() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(Const.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("jobs");
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(qname);
        final Xed xed = new Xed(document, xsdTypes);
        logger.finest(DocumentU.toString(xed.getDocument()));
        // navigate
        final Element jobs = xed.getXPather().getElement("/jobs");
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
        Assert.assertEquals(272, xml.length);
        Assert.assertEquals("cdf94a8b", CRCU.crc32String(xml));
    }

    public void testCreateUpdateJob() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(Const.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("jobs");
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(qname);
        final Xed xed = new Xed(document, xsdTypes);
        logger.finest(DocumentU.toString(xed.getDocument()));
        // navigate
        final Element jobs = xed.getXPather().getElement("/jobs");
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
        Assert.assertEquals(171, xml.length);
        Assert.assertEquals("d306a4dd", CRCU.crc32String(xml));
    }

    public void testCreateEnhancedJob() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(Const.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("jobs");
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(qname);
        final Xed xed = new Xed(document, xsdTypes);
        logger.finest(DocumentU.toString(xed.getDocument()));
        // navigate
        final Element jobs = xed.getXPather().getElement("/jobs");
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
        Assert.assertEquals(398, xml.length);
        Assert.assertEquals("19d4e9f8", CRCU.crc32String(xml));
    }

    public void testCreateUpdateEnhancedJob() throws Exception {
        // load model
        final URL urlInitial = ResourceU.resolve(Const.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("jobs");
        final Document document = new DocumentFactory(xsdTypes.getTypeDefinitions(), false).generateEmpty(qname);
        final Xed xed = new Xed(document, xsdTypes);
        logger.finest(DocumentU.toString(xed.getDocument()));
        // navigate
        final Element jobs = xed.getXPather().getElement("/jobs");
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
        Assert.assertEquals(234, xml.length);
        Assert.assertEquals("889ee76f", CRCU.crc32String(xml));
    }

    public static class Const {
        public static final String XSD = "io/github/greyp9/arwo/xsd/choiceNoNs/choiceNoNs.xsd";
    }
}
