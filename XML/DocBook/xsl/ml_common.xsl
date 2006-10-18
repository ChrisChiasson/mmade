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
<!--this template is originally from the DocBook project; it has been
	modified for caption handling-->
</xsl:param>
	<xsl:template match="caption/para">
		<xsl:choose>
			<xsl:when test="count(preceding-sibling::*)=0">
				<xsl:call-template name="captionparagraph">
					<xsl:with-param name="class">
						<xsl:if test="@role and $para.propagates.style != 0">
							<xsl:value-of select="@role"/>
						</xsl:if>
					</xsl:with-param>
					<xsl:with-param name="content">
						<xsl:if test="position() = 1 and parent::listitem">
							<xsl:call-template name="anchor">
								<xsl:with-param name="node" select="parent::listitem"/>
							</xsl:call-template>
						</xsl:if>
						<xsl:call-template name="anchor"/>
						<xsl:apply-templates/>
					</xsl:with-param>
				</xsl:call-template>
			</xsl:when>
			<xsl:otherwise>
				<xsl:call-template name="$paragraph">
					<xsl:with-param name="class">
						<xsl:if test="@role and $para.propagates.style != 0">
							<xsl:value-of select="@role"/>
						</xsl:if>
					</xsl:with-param>
					<xsl:with-param name="content">
						<xsl:if test="position() = 1 and parent::listitem">
							<xsl:call-template name="anchor">
								<xsl:with-param name="node" select="parent::listitem"/>
							</xsl:call-template>
						</xsl:if>
						<xsl:call-template name="anchor"/>
						<xsl:apply-templates/>
					</xsl:with-param>
				</xsl:call-template>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
</xsl:stylesheet>
<!--
	MMADE, a Mathematica DocBook Exporter
	The license and Copyright information for MMADE is included in rights.txt
	in the XML directory.
-->