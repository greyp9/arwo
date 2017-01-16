package io.github.greyp9.arwo.core.xed.session;

import io.github.greyp9.arwo.core.xml.QNameU;

import javax.xml.namespace.QName;

public class XedEntry {
    private final String title;
    private final String contextPath;
    private final String qname;
    private final String xmlPath;
    private final String xsdPath;
    private final String xsltPath;
    private final String trigger;

    public final String getTitle() {
        return title;
    }

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

    public final String getTrigger() {
        return trigger;
    }

    public XedEntry(final String contextPath, final XedEntry e) {
        this(e.getTitle(), contextPath, e.getQName(), e.getXmlPath(), e.getXsdPath(), e.getXsltPath(), e.getTrigger());
    }

    public XedEntry(final String title, final String contextPath, final QName qname,
                    final String xmlPath, final String xsdPath, final String xsltPath, final String trigger) {
        this(title, contextPath, QNameU.toStringColon(qname), xmlPath, xsdPath, xsltPath, trigger);
    }

    public XedEntry(final String title, final String contextPath, final String qname,
                    final String xmlPath, final String xsdPath, final String xsltPath, final String trigger) {
        this.title = title;
        this.contextPath = contextPath;
        this.qname = qname;
        this.xmlPath = xmlPath;
        this.xsdPath = xsdPath;
        this.xsltPath = xsltPath;
        this.trigger = trigger;
    }
}
