package io.github.greyp9.arwo.core.xed.suite.test.choice1.test;

import io.github.greyp9.arwo.core.app.test.TestApp;
import io.github.greyp9.arwo.core.res.ResourceU;
import io.github.greyp9.arwo.core.xml.QNameU;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstance;
import io.github.greyp9.arwo.core.xsd.instance.TypeInstanceTraversal;
import io.github.greyp9.arwo.core.xsd.model.XsdTypes;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import javax.xml.namespace.QName;
import java.net.URL;
import java.util.List;
import java.util.logging.Logger;

public class TypeInstanceTraversalTest {
    private final Logger logger = Logger.getLogger(getClass().getName());

    @BeforeEach
    public void setUp() throws Exception {
        //io.github.greyp9.arwo.core.logging.LoggerU.adjust(Logger.getLogger(""));
    }

    @Test
    public void testChoice1TraversalJobsToJob() throws Exception {
        final URL urlInitial = ResourceU.resolve(TestApp.Resources.XSD_CHOICE1);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("{urn:arwo:choice1}jobs");
        final TypeInstance typeInstanceJobs = xsdTypes.getTypeDefinitions().getElementTypes().get(qname.toString());
        final TypeInstance typeInstanceJob = typeInstanceJobs.getInstance("job");
        Assertions.assertNotNull(typeInstanceJob);

        final TypeInstanceTraversal traversalJobs = new TypeInstanceTraversal(typeInstanceJobs);
        final List<TypeInstance> listJob = traversalJobs.getForName("job");
        Assertions.assertEquals(1, listJob.size());
        Assertions.assertEquals("{urn:arwo:choice1}job", listJob.iterator().next().getQName().toString());
        logger.finest(listJob.toString());
        final List<TypeInstance> listEnhancedJob = traversalJobs.getForName("enhancedJob");
        Assertions.assertEquals(1, listEnhancedJob.size());
        Assertions.assertEquals(
                "{urn:arwo:choice1}enhancedJob", listEnhancedJob.iterator().next().getQName().toString());
        logger.finest(listEnhancedJob.toString());
    }

    @Test
    public void testChoice1TraversalJobToTaskA() throws Exception {
        final URL urlInitial = ResourceU.resolve(TestApp.Resources.XSD_CHOICE1);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("{urn:arwo:choice1}jobs");
        final TypeInstance typeInstanceJobs = xsdTypes.getTypeDefinitions().getElementTypes().get(qname.toString());
        final TypeInstance typeInstanceJob = typeInstanceJobs.getInstance("job");

        final TypeInstanceTraversal traversalJob = new TypeInstanceTraversal(typeInstanceJob);
        final List<TypeInstance> listTaskA = traversalJob.getForName("taskA");
        Assertions.assertEquals(2, listTaskA.size());
        Assertions.assertEquals("{http://www.w3.org/2001/XMLSchema}taskA/taskB/taskC",
                listTaskA.get(0).getQName().toString());
        Assertions.assertEquals("{urn:arwo:choice1}taskA", listTaskA.get(1).getQName().toString());
        logger.finest(listTaskA.toString());
    }

    @Test
    public void testChoice1TraversalJobToTaskC() throws Exception {
        final URL urlInitial = ResourceU.resolve(TestApp.Resources.XSD_CHOICE1);
        final XsdTypes xsdTypes = new XsdTypes(urlInitial);
        final QName qname = QNameU.getQName("{urn:arwo:choice1}jobs");
        final TypeInstance typeInstanceJobs = xsdTypes.getTypeDefinitions().getElementTypes().get(qname.toString());
        final TypeInstance typeInstanceJob = typeInstanceJobs.getInstance("job");

        final TypeInstanceTraversal traversalJob = new TypeInstanceTraversal(typeInstanceJob);
        final List<TypeInstance> listTaskC = traversalJob.getForName("taskC");
        Assertions.assertEquals(2, listTaskC.size());
        Assertions.assertEquals("{http://www.w3.org/2001/XMLSchema}taskA/taskB/taskC",
                listTaskC.get(0).getQName().toString());
        Assertions.assertEquals("{urn:arwo:choice1}taskC", listTaskC.get(1).getQName().toString());
        logger.finest(listTaskC.toString());
    }
}
