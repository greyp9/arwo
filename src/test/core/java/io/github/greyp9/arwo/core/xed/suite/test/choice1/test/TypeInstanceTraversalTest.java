package io.github.greyp9.arwo.core.xed.suite.test.choice1.test;

import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.xed.xsd.choice1.test.Choice1OpTest;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstanceTraversal;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import junit.framework.TestCase;
import org.junit.Assert;

import javax.xml.namespace.QName;
import java.net.URL;
import java.util.List;
import java.util.logging.Logger;

public class TypeInstanceTraversalTest extends TestCase {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @Override
    public void setUp() throws Exception {
        super.setUp();
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));
    }

    public void testChoice1TraversalJobsToJob() throws Exception {
        final URL urlInitial = ResourceU.resolve(Choice1OpTest.Const.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("{urn:arwo:choice1}jobs");
        final TypeInstance typeInstanceJobs = xsdTypes.getTypeDefinitions().getElementTypes().get(qname.toString());
        final TypeInstance typeInstanceJob = typeInstanceJobs.getInstance("job");
        Assert.assertNotNull(typeInstanceJob);

        final TypeInstanceTraversal traversalJobs = new TypeInstanceTraversal(typeInstanceJobs);
        final List<TypeInstance> listJob = traversalJobs.getForName("job");
        Assert.assertEquals(1, listJob.size());
        Assert.assertEquals("{urn:arwo:choice1}job", listJob.iterator().next().getQName().toString());
        logger.finest(listJob.toString());
        final List<TypeInstance> listEnhancedJob = traversalJobs.getForName("enhancedJob");
        Assert.assertEquals(1, listEnhancedJob.size());
        Assert.assertEquals("{urn:arwo:choice1}enhancedJob", listEnhancedJob.iterator().next().getQName().toString());
        logger.finest(listEnhancedJob.toString());
    }

    public void testChoice1TraversalJobToTaskA() throws Exception {
        final URL urlInitial = ResourceU.resolve(Choice1OpTest.Const.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("{urn:arwo:choice1}jobs");
        final TypeInstance typeInstanceJobs = xsdTypes.getTypeDefinitions().getElementTypes().get(qname.toString());
        final TypeInstance typeInstanceJob = typeInstanceJobs.getInstance("job");

        final TypeInstanceTraversal traversalJob = new TypeInstanceTraversal(typeInstanceJob);
        final List<TypeInstance> listTaskA = traversalJob.getForName("taskA");
        Assert.assertEquals(2, listTaskA.size());
        Assert.assertEquals("{http://www.w3.org/2001/XMLSchema}taskA/taskB/taskC",
                listTaskA.get(0).getQName().toString());
        Assert.assertEquals("{urn:arwo:choice1}taskA", listTaskA.get(1).getQName().toString());
        logger.finest(listTaskA.toString());
    }

    public void testChoice1TraversalJobToTaskC() throws Exception {
        final URL urlInitial = ResourceU.resolve(Choice1OpTest.Const.XSD);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("{urn:arwo:choice1}jobs");
        final TypeInstance typeInstanceJobs = xsdTypes.getTypeDefinitions().getElementTypes().get(qname.toString());
        final TypeInstance typeInstanceJob = typeInstanceJobs.getInstance("job");

        final TypeInstanceTraversal traversalJob = new TypeInstanceTraversal(typeInstanceJob);
        final List<TypeInstance> listTaskC = traversalJob.getForName("taskC");
        Assert.assertEquals(2, listTaskC.size());
        Assert.assertEquals("{http://www.w3.org/2001/XMLSchema}taskA/taskB/taskC",
                listTaskC.get(0).getQName().toString());
        Assert.assertEquals("{urn:arwo:choice1}taskC", listTaskC.get(1).getQName().toString());
        logger.finest(listTaskC.toString());
    }
}
