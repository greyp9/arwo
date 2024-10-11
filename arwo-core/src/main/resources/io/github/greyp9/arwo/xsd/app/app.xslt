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

    <xsl:template match='/xsd:schema/xsd:complexType[@name="sshServerType"]//xsd:element[@name="publicKey"]'>
        <xsl:copy>
            <xsl:attribute name='xed:rows'>6</xsl:attribute>
            <xsl:attribute name='xed:cols'>120</xsl:attribute>
            <xsl:attribute name='xed:hideInTable'>true</xsl:attribute>
            <xsl:apply-templates select='@*|node()'/>
        </xsl:copy>
    </xsl:template>

    <xsl:template match='/xsd:schema/xsd:complexType[@name="webdavServerType" or @name="smtpServerType" or
    @name="imapServerType" or @name="pop3ServerType"]//xsd:element[@name="certificate"]'>
        <xsl:copy>
            <xsl:attribute name='xed:rows'>12</xsl:attribute>
            <xsl:attribute name='xed:cols'>80</xsl:attribute>
            <xsl:attribute name='xed:hideInTable'>true</xsl:attribute>
            <xsl:apply-templates select='@*|node()'/>
        </xsl:copy>
    </xsl:template>

    <xsl:template match='/xsd:schema/xsd:complexType[@name="jdbcServerType"]//xsd:element[@name="url"]'>
        <xsl:copy>
            <xsl:attribute name='xed:size'>132</xsl:attribute>
            <xsl:apply-templates select='@*|node()'/>
        </xsl:copy>
    </xsl:template>

    <xsl:template match='/xsd:schema/xsd:complexType[@name="sshAuthPasswordType" or @name="cifsServerType" or
    @name="webdavServerType" or @name="jdbcServerType" or @name="smtpServerType" or @name="imapServerType" or
    @name="pop3ServerType"]/xsd:sequence/xsd:element[@name="password"]'>
        <xsl:copy>
            <xsl:attribute name='xed:transform'>AES/GCM/NoPadding</xsl:attribute>
            <xsl:attribute name='xed:parameterSpec'>GCMParameterSpec</xsl:attribute>
            <xsl:apply-templates select='@*|node()'/>
        </xsl:copy>
    </xsl:template>

    <xsl:template match='/xsd:schema/xsd:complexType[@name="sshAuthPublicKeyType"]/xsd:sequence/xsd:element[@name="privateKey"]'>
        <xsl:copy>
            <xsl:attribute name='xed:transform'>AES/GCM/NoPadding</xsl:attribute>
            <xsl:attribute name='xed:parameterSpec'>GCMParameterSpec</xsl:attribute>
            <xsl:attribute name='xed:rows'>12</xsl:attribute>
            <xsl:attribute name='xed:cols'>120</xsl:attribute>
            <xsl:attribute name='xed:hideInTable'>true</xsl:attribute>
            <xsl:apply-templates select='@*|node()'/>
        </xsl:copy>
    </xsl:template>

</xsl:stylesheet>
