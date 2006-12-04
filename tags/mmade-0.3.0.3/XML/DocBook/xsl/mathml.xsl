<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns:mml="http://www.w3.org/1998/Math/MathML"
	xmlns:mathematica="http://www.wolfram.com/XML/"
	version="1.0">
	<xsl:template match="mml:*">
		<xsl:element name="{local-name(.)}" namespace="{namespace-uri(.)}">
			<xsl:for-each select="@*[namespace-uri()='' or namespace-uri()=namespace-uri(parent::*)]">
				<xsl:copy/>
			</xsl:for-each>
			<xsl:apply-templates/>
		</xsl:element>
	</xsl:template>
	<xsl:template match="mml:semantics">
		<xsl:apply-templates select="mml:*[1]"/>
	</xsl:template>
</xsl:stylesheet>