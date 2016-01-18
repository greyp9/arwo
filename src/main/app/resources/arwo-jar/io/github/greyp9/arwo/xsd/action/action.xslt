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

    <xsl:template match='/xsd:schema/xsd:complexType[@name="commandType"]//xsd:element[@name="command"]'>
        <xsl:copy>
            <xsl:attribute name='xed:rows'>12</xsl:attribute>
            <xsl:attribute name='xed:cols'>80</xsl:attribute>
            <xsl:attribute name='xed:hideName'>true</xsl:attribute>
            <xsl:apply-templates select='@*|node()'/>
        </xsl:copy>
    </xsl:template>

    <xsl:template match='/xsd:schema/xsd:complexType[@name="fileEditType"]//xsd:element[@name="file"]'>
        <xsl:copy>
            <xsl:attribute name='xed:rows'>25</xsl:attribute>
            <xsl:attribute name='xed:cols'>132</xsl:attribute>
            <xsl:attribute name='xed:hideName'>true</xsl:attribute>
            <xsl:apply-templates select='@*|node()'/>
        </xsl:copy>
    </xsl:template>

    <xsl:template match='/xsd:schema/xsd:complexType[@name="sqlType"]//xsd:element[@name="sql"]'>
        <xsl:copy>
            <xsl:attribute name='xed:rows'>12</xsl:attribute>
            <xsl:attribute name='xed:cols'>80</xsl:attribute>
            <xsl:attribute name='xed:hideName'>true</xsl:attribute>
            <xsl:apply-templates select='@*|node()'/>
        </xsl:copy>
    </xsl:template>

    <xsl:template match='/xsd:schema/xsd:complexType[@name="mailType"]//xsd:element[@name="body"]'>
        <xsl:copy>
            <xsl:attribute name='xed:rows'>12</xsl:attribute>
            <xsl:attribute name='xed:cols'>80</xsl:attribute>
            <xsl:apply-templates select='@*|node()'/>
        </xsl:copy>
    </xsl:template>

    <xsl:template match='/xsd:schema/xsd:complexType[@name="fileNewType"]//xsd:element[@name="file"]'>
        <xsl:copy>
            <xsl:attribute name='xed:rows'>25</xsl:attribute>
            <xsl:attribute name='xed:cols'>132</xsl:attribute>
            <xsl:apply-templates select='@*|node()'/>
        </xsl:copy>
    </xsl:template>

</xsl:stylesheet>
