<?xml version='1.0' encoding='UTF-8'?>
<xsl:stylesheet version='1.0' xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

    <xsl:output method='xml' encoding='UTF-8' indent='yes'/>

    <xsl:preserve-space elements='*'/>

    <xsl:template match='@*|node()'>
        <xsl:copy>
            <xsl:apply-templates select='@*|node()'/>
        </xsl:copy>
    </xsl:template>

    <xsl:template match='/tomcat-users'>
        <tomcat-users>
            <xsl:copy-of select='@*'/>
            <user username='arwo' password='arwo' roles='manager'/>
            <xsl:apply-templates select='@*|node()'/>
        </tomcat-users>
    </xsl:template>

</xsl:stylesheet>
