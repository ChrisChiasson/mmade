<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns:fo="http://www.w3.org/1999/XSL/Format" version="1.0">
	<xsl:import
		href="http://docbook.sourceforge.net/release/xsl/current/fo/docbook.xsl"/>
	<xsl:import href="common.xsl"/>
	<!--xsl:param name="ulink.show">0</xsl:param-->
	<!--xsl:param name="alignment">left</xsl:param-->
	<!--xsl:param name="fop1.extensions">1</xsl:param-->
	<!--<xsl:param name="fop.extensions">1</xsl:param>-->
	<xsl:param name="xep.extensions">1</xsl:param>
	<!--included Mathematica source is usually 80 characters wide
	    so having a small font size is probably necessary-->
	<xsl:attribute-set name="monospace.verbatim.properties">
		<xsl:attribute name="font-size">8pt</xsl:attribute>
	</xsl:attribute-set>
	<xsl:template match="caption/para">
		<xsl:choose>
			<xsl:when test="count(preceding-sibling::*)=0">
				<fo:block xsl:use-attribute-sets="normal.para.spacing">
					<xsl:call-template name="anchor"/>
					<fo:inline font-weight="bold">Caption: </fo:inline>
					<xsl:apply-templates/>
				</fo:block>
			</xsl:when>
			<xsl:otherwise>
				<fo:block xsl:use-attribute-sets="normal.para.spacing">
					<xsl:call-template name="anchor"/>
					<xsl:apply-templates/>
				</fo:block>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
</xsl:stylesheet>
<!--
	MMADE, a Mathematica DocBook Exporter
	The license and Copyright information for MMADE is included in rights.txt
	in the XML directory.
-->