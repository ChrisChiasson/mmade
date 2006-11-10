<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
	<!--numbering or lettering of chapters (1 is that default)-->
	<xsl:param name="chapter.autolabel">1</xsl:param>
	<!--restart chapter numbering at each part? (0 is the default)-->
	<xsl:param name="label.from.part">0</xsl:param>
	<!--indication of part number in hot link automatic text (0 default)-->
	<xsl:param name="component.label.includes.part.label">1</xsl:param>
	<!--section numbering (so it will be used in hot text) (0 default)-->
	<xsl:param name="section.autolabel">1</xsl:param>
	<!--follow xml:base-->
	<xsl:param name="keep.relative.image.uris">0</xsl:param>
	<!--olinking on (defaluts to no)-->
	<xsl:param name="collect.xref.targets">yes</xsl:param>
	<!--the single entry in target.xml points to target.db-->
	<xsl:param name="target.database.document">olinkdb.xml</xsl:param>
	<!--set the doc id to self to match target.xml-->
	<xsl:param name="current.docid">self</xsl:param>
	<!--if draft mode is on, use a local image instead of the one on sourceforge-->
	<xsl:param name="draft.watermark.image">draft.png</xsl:param>
	<!--draft mode off (defaults to maybe)-->
	<xsl:param name="draft.mode">no</xsl:param>
	<!--include, for example, chapter numbers in section numbers-->
	<xsl:param name="section.label.includes.component.label">1</xsl:param>
	<!--include part numbers if there are any parts to the book-->
	<xsl:param name="component.label.includes.part.label">1</xsl:param>
	<!--some abbreviations for common xrefs-->
	<xsl:param name="local.l10n.xml" select="document('')"/>
	<l:i18n xmlns:l="http://docbook.sourceforge.net/xmlns/l10n/1.0">
		<l:l10n language="en">
			<l:context name="xref-number">
				<l:template name="chapter" text="Ch.&#160;%n"/>
				<l:template name="equation" text="Eq.&#160;%n"/>
				<l:template name="example" text="Ex.&#160;%n"/>
				<l:template name="figure" text="Fig.&#160;%n"/>
				<l:template name="section" text="&#167;&#160;%n"/>				
			</l:context>				
			<l:context name="xref-number-and-title">
				<l:template name="chapter" text="Ch.&#160;%n, %t"/>
				<l:template name="equation" text="Eq.&#160;%n, &#8220;%t&#8221;"/>
				<l:template name="example" text="Ex.&#160;%n, &#8220;%t&#8221;"/>
				<l:template name="figure" text="Fig.&#160;%n, &#8220;%t&#8221;"/>
				<l:template name="section" text="&#167;&#160;%n, &#8220;%t&#8221;"/>		
			</l:context>    
		</l:l10n>
	</l:i18n>
</xsl:stylesheet>
<!--
	MMADE, a Mathematica DocBook Exporter
	The license and Copyright information for MMADE is included in rights.txt
	in the XML directory.
-->