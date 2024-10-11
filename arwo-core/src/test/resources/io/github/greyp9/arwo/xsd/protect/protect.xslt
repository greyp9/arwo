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

    <xsl:template match='/xsd:schema/xsd:complexType[@name="accountTypePBE"]/xsd:attribute[@name="password"]'>
        <xsl:copy>
            <xsl:attribute name='xed:pbe'>PBKDF2WithHmacSHA1</xsl:attribute>
            <xsl:attribute name='xed:salt'>AAECAwQFBgc=</xsl:attribute>
            <xsl:attribute name='xed:algorithm'>AES</xsl:attribute>
            <xsl:attribute name='xed:transform'>AES/CBC/PKCS5Padding</xsl:attribute>
            <xsl:attribute name='xed:parameterSpec'>IvParameterSpec</xsl:attribute>
            <xsl:attribute name='xed:iterations'>1000</xsl:attribute>
            <xsl:attribute name='xed:keysize'>128</xsl:attribute>
            <xsl:apply-templates select='@*|node()'/>
        </xsl:copy>
    </xsl:template>

    <xsl:template match='/xsd:schema/xsd:complexType[@name="accountTypeGCM"]/xsd:attribute[@name="password"]'>
        <xsl:copy>
            <xsl:attribute name='xed:transform'>AES/GCM/NoPadding</xsl:attribute>
            <xsl:attribute name='xed:parameterSpec'>GCMParameterSpec</xsl:attribute>
            <xsl:apply-templates select='@*|node()'/>
        </xsl:copy>
    </xsl:template>

</xsl:stylesheet>
