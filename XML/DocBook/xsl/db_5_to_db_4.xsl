<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns:exsl="http://exslt.org/common"
	xmlns:ng="http://docbook.org/docbook-ng"
	xmlns:db="http://docbook.org/ns/docbook"
	exclude-result-prefixes="db ng exsl"
	version='1.0'>
	<!--This file is used with the permission of its author, Bob Stayton.-->
	<!--I, Chris Chiasson, modified the line below to use the official docbook-xsl url.-->
	<xsl:import href="http://docbook.sourceforge.net/release/xsl/current/fo/docbook.xsl"/>
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
<!--
	MMADE, a Mathematica DocBook Exporter
	The license and Copyright information for MMADE is included in rights.txt
	in the XML directory.
-->