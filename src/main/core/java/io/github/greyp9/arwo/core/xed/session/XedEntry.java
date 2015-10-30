package io.github.greyp9.arwo.core.xed.session;

public class XedEntry {
    private final String contextPath;
    private final String xmlPath;
    private final String xsdPath;
    private final String xsltPath;

    public final String getContextPath() {
        return contextPath;
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

    public XedEntry(final String contextPath, final String xmlPath, final String xsdPath, final String xsltPath) {
        this.contextPath = contextPath;
        this.xmlPath = xmlPath;
        this.xsdPath = xsdPath;
        this.xsltPath = xsltPath;
    }
}
