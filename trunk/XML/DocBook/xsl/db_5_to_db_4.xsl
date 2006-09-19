<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns:exsl="http://exslt.org/common"
	xmlns:ng="http://docbook.org/docbook-ng"
	xmlns:db="http://docbook.org/ns/docbook"
	exclude-result-prefixes="db ng exsl"
	version='1.0'>
	<!--this file is used with permission of Bob Stayton-->
	<!--I, Chris Chiasson, modified the line below to use the official docbook-xsl url-->
	<xsl:import href="http://docbook.sourceforge.net/release/xsl/current/fo/profile-docbook.xsl"/>
	<xsl:template match="/">
		<xsl:if test="function-available('exsl:node-set')
			and (*/self::ng:* or */self::db:*)">
			<xsl:variable name="nons">
				<xsl:apply-templates mode="stripNS"/>
			</xsl:variable>
			<xsl:copy-of select="exsl:node-set($nons)"/>
		</xsl:if>
	</xsl:template>
</xsl:stylesheet>