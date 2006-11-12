<?xml version='1.0'?>
<xsl:stylesheet
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns="http://www.w3.org/1999/xhtml"
    version="1.0">
	<xsl:import href="http://docbook.sourceforge.net/release/xsl/current/xhtml/chunk.xsl"/>
	<xsl:import href="common.xsl"/>
	<xsl:import href="chunk_common.xsl"/>
	<xsl:import href="ml_common.xsl"/>
	<xsl:import href="xhtml_common.xsl"/>
	<xsl:param name="html.ext">.xhtml</xsl:param>
	<xsl:param name="chunker.output.doctype-public" select="'-//W3C//DTD XHTML 1.1 plus MathML 2.0 plus SVG 1.1//EN'"/>
	<xsl:param name="chunker.output.doctype-system" select="'http://www.w3.org/Math/DTD/mathml2/xhtml-math11-f.dtd'"/>
</xsl:stylesheet>
<!--
	MMADE, a Mathematica DocBook Exporter
	The license and Copyright information for MMADE is included in rights.txt
	in the XML directory.
-->