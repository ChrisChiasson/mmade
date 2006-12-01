<?xml version='1.0'?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:stext="http://nwalsh.com/xslt/ext/com.nwalsh.saxon.TextFactory" xmlns:simg="http://nwalsh.com/xslt/ext/com.nwalsh.saxon.ImageIntrinsics" xmlns:ximg="xaln://com.nwalsh.xalan.ImageIntrinsics" xmlns:xtext="com.nwalsh.xalan.Text" xmlns:lxslt="http://xml.apache.org/xslt" xmlns="http://www.w3.org/1999/xhtml" exclude-result-prefixes="xlink stext xtext lxslt simg ximg" extension-element-prefixes="stext xtext">
	<xsl:import href="xhtml-process-image.xsl"/>
	
	<!--xhtml strict conformance parameters-->
	
	<xsl:param name="css.decoration">0</xsl:param>
	<xsl:param name="html.longdesc">0</xsl:param>
	<xsl:param name="ulink.target"></xsl:param>
	<xsl:param name="use.viewport">0</xsl:param>
	
	<!--end xhtml strict conformance parameters-->
	
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
	<!--this is the xhtml imagedata template from the DocBook project
		it is patched to handle SVG and MathML markup children of imagedata
		from DocBook 5
		except for the modifications, this template did not originate from the
		MMADE project or its authors
	-->
	<xsl:template match="imagedata">
		<xsl:choose>
			<xsl:when xmlns:svg="http://www.w3.org/2000/svg" test="svg:*">
				<xsl:apply-templates/>
			</xsl:when>
			<xsl:when xmlns:mml="http://www.w3.org/1998/Math/MathML" test="mml:*">
				<xsl:apply-templates/>
			</xsl:when>
			<xsl:otherwise>
				<xsl:apply-imports/>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
</xsl:stylesheet>
<!--
	MMADE, a Mathematica DocBook Exporter
	The license and Copyright information for MMADE is included in rights.txt
	in the XML directory.
-->