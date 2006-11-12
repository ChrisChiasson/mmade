<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns:mml="http://www.w3.org/1998/Math/MathML"
	xmlns:mathematica="http://www.wolfram.com/XML/"
	version="1.0">
	<xsl:template match="mml:*">
		<xsl:element name="{local-name(.)}" namespace="{namespace-uri(.)}">
			<xsl:for-each select="@*[namespace-uri()=namespace-uri(parent::self)]">
				<xsl:copy/>
			</xsl:for-each>
			<xsl:apply-templates/>
		</xsl:element>
	</xsl:template>
</xsl:stylesheet>