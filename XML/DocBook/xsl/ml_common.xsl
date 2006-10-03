<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
		xmlns="http://www.w3.org/1999/xhtml" version="1.0">
<xsl:param name="html.stylesheet">ml.css</xsl:param>
<xsl:param name="generate.toc">
appendix  toc,title
article/appendix  nop
article   nop
book      toc,title,figure,table,example,equation
chapter   nop
part      toc
preface   nop
qandadiv  nop
qandaset  nop
reference toc,title
sect1     nop
sect2     nop
sect3     nop
sect4     nop
sect5     nop
section   nop
set       toc,title
</xsl:param>
</xsl:stylesheet>
<!--
	MMADE, a Mathematica DocBook Exporter
	The license and Copyright information for MMADE is included in rights.txt
	in the XML directory.
-->