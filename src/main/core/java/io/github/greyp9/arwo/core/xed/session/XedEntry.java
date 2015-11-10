package io.github.greyp9.arwo.core.xed.session;

import javax.xml.namespace.QName;

public class XedEntry {
    private final String contextPath;
    private final String qname;
    private final String xmlPath;
    private final String xsdPath;
    private final String xsltPath;

    public final String getContextPath() {
        return contextPath;
    }

    public final String getQName() {
        return qname;
    }

    public final String getXmlPath() {
        return xmlPath;
    }

    public final String getXsdPath() {
        return xsdPath;
    }

    public final String getXsltPath() {
        return xsltPath;
    }

    public XedEntry(final String contextPath, final QName qname,
                    final String xmlPath, final String xsdPath, final String xsltPath) {
        this(contextPath, qname.toString(), xmlPath, xsdPath, xsltPath);
    }

    public XedEntry(final String contextPath, final String qname,
                    final String xmlPath, final String xsdPath, final String xsltPath) {
        this.contextPath = contextPath;
        this.qname = qname;
        this.xmlPath = xmlPath;
        this.xsdPath = xsdPath;
        this.xsltPath = xsltPath;
    }
}
