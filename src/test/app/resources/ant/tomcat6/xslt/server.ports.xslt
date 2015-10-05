<?xml version='1.0' encoding='UTF-8'?>
<xsl:stylesheet version='1.0' xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>

    <xsl:output method='xml' encoding='UTF-8' indent='yes'/>

    <xsl:preserve-space elements='*'/>

    <xsl:template match='@*|node()'>
        <xsl:copy>
            <xsl:apply-templates select='@*|node()'/>
        </xsl:copy>
    </xsl:template>

    <!-- shutdown port -->
    <xsl:template match='/Server/@port'>
        <xsl:attribute name='port'>8265</xsl:attribute>
    </xsl:template>

    <xsl:template match='/Server/@shutdown'>
        <xsl:attribute name='shutdown'>GET /SHUTDOWN HTTP/1.1</xsl:attribute>
    </xsl:template>

    <!-- http port -->
    <xsl:template match='/Server/Service/Connector[@protocol = &apos;HTTP/1.1&apos;]/@port'>
        <xsl:attribute name='port'>8260</xsl:attribute>
    </xsl:template>

    <!-- https port -->
    <xsl:template match='/Server/Service/Connector[@SSLEnabled = &apos;true&apos;]/@port'>
        <xsl:attribute name='port'>8261</xsl:attribute>
    </xsl:template>

    <xsl:template match='//Connector[@redirectPort = &apos;8443&apos;]/@redirectPort'>
        <xsl:attribute name='redirectPort'>8261</xsl:attribute>
    </xsl:template>

    <!-- ajp port -->
    <xsl:template match='/Server/Service/Connector[@protocol = &apos;AJP/1.3&apos;]/@port'>
        <xsl:attribute name='port'>8269</xsl:attribute>
    </xsl:template>

    <!-- keystore -->
    <xsl:template match='/Server/Service/Connector[@SSLEnabled = &apos;true&apos;]'>
        <xsl:copy>
            <xsl:attribute name='keystoreFile'>${ssl.dir}/server.pkcs12</xsl:attribute>
            <xsl:attribute name='keystoreType'>pkcs12</xsl:attribute>
            <xsl:attribute name='keystorePass'>arwo</xsl:attribute>
            <!--<xsl:attribute name='keyAlias'>localhost</xsl:attribute>-->
            <xsl:attribute name='truststoreFile'>${ssl.dir}/client.pkcs12</xsl:attribute>
            <xsl:attribute name='truststoreType'>pkcs12</xsl:attribute>
            <xsl:attribute name='truststorePass'>arwo</xsl:attribute>
            <!--Workaround for Tomcat: SSL TLS Logjam vulnerability - Powered by Kayako Help Desk Software-->
            <!--http://support.filecatalyst.com/index.php?/Knowledgebase/Article/View/277/0/workaround-for-tomcat-ssl-tls-logjam-vulnerability-->
            <xsl:attribute name='ciphers'>TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA256,TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA,
                TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA384,TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA,TLS_ECDHE_RSA_WITH_RC4_128_SHA,
                TLS_RSA_WITH_AES_128_CBC_SHA256,TLS_RSA_WITH_AES_128_CBC_SHA,TLS_RSA_WITH_AES_256_CBC_SHA256,
                TLS_RSA_WITH_AES_256_CBC_SHA,SSL_RSA_WITH_RC4_128_SHA
            </xsl:attribute>
            <xsl:apply-templates select='@*|node()'/>
        </xsl:copy>
    </xsl:template>

    <!-- log accesses -->
    <xsl:template match='/Server/Service/Engine/Host'>
        <Host>
            <xsl:copy-of select='@*'/>
            <xsl:apply-templates select='node()'/>
            <Valve className='org.apache.catalina.valves.AccessLogValve' directory='logs'
                   prefix='localhost_access_log.' suffix='.txt' pattern='common' resolveHosts='false'/>
        </Host>
    </xsl:template>

</xsl:stylesheet>
