<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:output method="html"
            media-type="text/html"
            encoding="ISO-8859-1"
            indent="no"
            doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN"
            doctype-system="http://www.w3.org/TR/html4/loose.dtd"/>

	<!--this template is originally from the DocBook project; it has been
		modified for caption handling-->
	<xsl:template name="captionparagraph">
		<xsl:param name="class" select="''"/>
		<xsl:param name="content"/>
		
		<xsl:variable name="p">
			<p>
				<xsl:call-template name="dir"/>
				<xsl:if test="$class != ''">
					<xsl:attribute name="class">
						<xsl:value-of select="$class"/>
					</xsl:attribute>
				</xsl:if>
				<!--line added-->
				<b><xsl:text>Caption: </xsl:text></b>
				<xsl:copy-of select="$content"/>
			</p>
		</xsl:variable>
		
		<xsl:choose>
			<xsl:when test="$html.cleanup != 0">
				<xsl:call-template name="unwrap.p">
					<xsl:with-param name="p" select="$p"/>
				</xsl:call-template>
			</xsl:when>
			<xsl:otherwise>
				<xsl:copy-of select="$p"/>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	<!--this template is originally from the DocBook project; it has been
		modified for question handling-->
	<xsl:template name="questionparagraph">
		<xsl:param name="class" select="''"/>
		<xsl:param name="content"/>
		
		<xsl:variable name="p">
			<p>
				<xsl:call-template name="dir"/>
				<xsl:if test="$class != ''">
					<xsl:attribute name="class">
						<xsl:value-of select="$class"/>
					</xsl:attribute>
				</xsl:if>
				<!--line added-->
				<b><xsl:text>Q: </xsl:text></b>
				<xsl:copy-of select="$content"/>
			</p>
		</xsl:variable>
		
		<xsl:choose>
			<xsl:when test="$html.cleanup != 0">
				<xsl:call-template name="unwrap.p">
					<xsl:with-param name="p" select="$p"/>
				</xsl:call-template>
			</xsl:when>
			<xsl:otherwise>
				<xsl:copy-of select="$p"/>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	<!--this template is originally from the DocBook project; it has been
		modified for answer handling-->
	<xsl:template name="answerparagraph">
		<xsl:param name="class" select="''"/>
		<xsl:param name="content"/>
		
		<xsl:variable name="p">
			<p>
				<xsl:call-template name="dir"/>
				<xsl:if test="$class != ''">
					<xsl:attribute name="class">
						<xsl:value-of select="$class"/>
					</xsl:attribute>
				</xsl:if>
				<!--line added-->
				<b><xsl:text>A: </xsl:text></b>
				<xsl:copy-of select="$content"/>
			</p>
		</xsl:variable>
		
		<xsl:choose>
			<xsl:when test="$html.cleanup != 0">
				<xsl:call-template name="unwrap.p">
					<xsl:with-param name="p" select="$p"/>
				</xsl:call-template>
			</xsl:when>
			<xsl:otherwise>
				<xsl:copy-of select="$p"/>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
</xsl:stylesheet> 
<!--
	MMADE, a Mathematica DocBook Exporter
	The license and Copyright information for MMADE is included in rights.txt
	in the XML directory.
-->