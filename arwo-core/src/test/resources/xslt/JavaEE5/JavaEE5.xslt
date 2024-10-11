<?xml version='1.0' encoding='UTF-8'?>
<xsl:stylesheet version='1.0'
                xmlns:xed='urn:xed:xed'
                xmlns:xsd='http://www.w3.org/2001/XMLSchema'
                xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

    <xsl:output method='xml' encoding='UTF-8' indent='yes'/>

    <xsl:preserve-space elements='*'/>

    <xsl:template match='@*|node()'>
        <xsl:copy>
            <xsl:apply-templates select='@*|node()'/>
        </xsl:copy>
    </xsl:template>

    <xsl:template match='/xsd:schema[@targetNamespace="http://java.sun.com/xml/ns/javaee"]
        //xsd:element[@name="encoding"]'>
        <xsl:copy>
            <xsl:attribute name="xed:default">UTF-8</xsl:attribute>
            <xsl:apply-templates select='@*|node()'/>
        </xsl:copy>
    </xsl:template>

    <xsl:template match='/xsd:schema[@targetNamespace="http://java.sun.com/xml/ns/javaee"]//xsd:element[
        @name="facet-name" or @name="managed-bean-name" or @name="referenced-bean-name" or @name="name-given" or
        @name="field-name" or @name="injection-target-name" or @name="lifecycle-callback-method" or
        @name="method-name" or @name="abstract-schema-name" or @name="name"]'>
        <xsl:copy>
            <xsl:attribute name="xed:default">HelloWorld</xsl:attribute>
            <xsl:apply-templates select='@*|node()'/>
        </xsl:copy>
    </xsl:template>

    <xsl:template match='/xsd:schema[@targetNamespace="http://java.sun.com/xml/ns/javaee"]
        //xsd:element[@name="http-method"]'>
        <xsl:copy>
            <xsl:attribute name="xed:default">GET</xsl:attribute>
            <xsl:apply-templates select='@*|node()'/>
        </xsl:copy>
    </xsl:template>

    <xsl:template match='/xsd:schema[@targetNamespace="http://java.sun.com/xml/ns/javaee"]
        //xsd:element[@name="locale" or @name="default-locale" or @name="supported-locale"]'>
        <xsl:copy>
            <xsl:attribute name="xed:default">en</xsl:attribute>
            <xsl:apply-templates select='@*|node()'/>
        </xsl:copy>
    </xsl:template>

    <xsl:template match='/xsd:schema[@targetNamespace="http://java.sun.com/xml/ns/javaee"]
        //xsd:element[@name="mime-type"]'>
        <xsl:copy>
            <xsl:attribute name="xed:default">text/plain</xsl:attribute>
            <xsl:apply-templates select='@*|node()'/>
        </xsl:copy>
    </xsl:template>

    <xsl:template match='/xsd:schema[@targetNamespace="http://java.sun.com/xml/ns/javaee"]
        //xsd:element[@name="error-code"]'>
        <xsl:copy>
            <xsl:attribute name="xed:default">404</xsl:attribute>
            <xsl:apply-templates select='@*|node()'/>
        </xsl:copy>
    </xsl:template>

    <xsl:template match='/xsd:schema[@targetNamespace="http://java.sun.com/xml/ns/javaee"]
        //xsd:element[@name="tlib-version"]'>
        <xsl:copy>
            <xsl:attribute name="xed:default">1.0</xsl:attribute>
            <xsl:apply-templates select='@*|node()'/>
        </xsl:copy>
    </xsl:template>

    <xsl:template match='/xsd:schema[@targetNamespace="http://java.sun.com/xml/ns/javaee"]
        //xsd:element[@name="service-name-pattern"]'>
        <xsl:copy>
            <xsl:attribute name="xed:default">xmlns:foo</xsl:attribute>
            <xsl:apply-templates select='@*|node()'/>
        </xsl:copy>
    </xsl:template>

    <xsl:template match='/xsd:schema[@targetNamespace="http://java.sun.com/xml/ns/javaee"]
        //xsd:element[@name="from-action"]'>
        <xsl:copy>
            <xsl:attribute name="xed:default">#{foo}</xsl:attribute>
            <xsl:apply-templates select='@*|node()'/>
        </xsl:copy>
    </xsl:template>

</xsl:stylesheet>
