<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns:fo="http://www.w3.org/1999/XSL/Format"
	xmlns:mml="http://www.w3.org/1998/Math/MathML"
	xmlns:mathematica="http://www.wolfram.com/XML/" version="1.0">
	<xsl:import
		href="http://docbook.sourceforge.net/release/xsl/current/fo/docbook.xsl"/>
	<xsl:import href="common.xsl"/>
	<xsl:import href="fo-process-image.xsl"/>
	<!--line break handling-->
	<xsl:template match="processing-instruction('lb')">
		<fo:block/>
	</xsl:template>
	<!--xsl:param name="ulink.show">0</xsl:param-->
	<!--xsl:param name="alignment">left</xsl:param-->
	<!--xsl:param name="fop1.extensions">1</xsl:param-->
	<!--<xsl:param name="fop.extensions">1</xsl:param>-->
	<xsl:param name="xep.extensions">1</xsl:param>
	<!--we need to enable some symbol fonts-->
	<xsl:param name="symbol.font.family"
		select="'Symbol,ZapfDingbats,Arial Unicode MS, Lucida Sans Unicode'"/>
	<!--included Mathematica source is usually 80 characters wide
	    so having a small font size is probably necessary-->
	<xsl:attribute-set name="monospace.verbatim.properties">
		<xsl:attribute name="font-size">8pt</xsl:attribute>
	</xsl:attribute-set>
	<xsl:attribute-set name="root.properties">
		<xsl:attribute name="line-height-shift-adjustment"
		>consider-shifts</xsl:attribute>
	</xsl:attribute-set>
	<!--add support for tightfofit style tables-->
	<xsl:attribute-set name="table.properties">
		<xsl:attribute name="start-indent">
			<xsl:choose>
				<xsl:when test="@tabstyle='tightfofit'">0pt</xsl:when>
				<xsl:otherwise>inherit</xsl:otherwise>
			</xsl:choose>
		</xsl:attribute>
	</xsl:attribute-set>
	<xsl:template name="table.cell.block.properties">
		<xsl:if test="ancestor::thead or ancestor::tfoot">
			<xsl:attribute name="font-weight">bold</xsl:attribute>
		</xsl:if>
		<xsl:if
			test="ancestor-or-self::table[1]/@tabstyle='tightfofit' or
			ancestor-or-self::informaltable[1]/@tabstyle='tightfofit'">
			<xsl:attribute name="font-size">8pt</xsl:attribute>
		</xsl:if>
	</xsl:template>
	<!--this stylesheet is different than the "main" mathml.xsl because
		I am trying to work around a problem where SVGMath seems to ignore
		the fallback fonts when fontfamily is specified-->
	<xsl:template match="mml:*">
		<xsl:element name="{local-name(.)}" namespace="{namespace-uri(.)}">
			<xsl:for-each
				select="@*[local-name()!='fontfamily' and (namespace-uri()='' or namespace-uri()=namespace-uri(parent::*))]">
				<xsl:copy/>
			</xsl:for-each>
			<xsl:apply-templates/>
		</xsl:element>
	</xsl:template>
	<xsl:template match="mml:semantics">
		<xsl:apply-templates select="mml:*[1]"/>
	</xsl:template>
	<!--This is the fo table.footnote.block template from the DocBook project.
		It is patched to handle caption markup children of table from DocBook 5.
		This is a rather weird place to add caption support; when the main
		stylesheets add the support, MMADE users will probably end up with
		double captions until this template is turned off.
		Except for the modifications, this template did not originate from the
		MMADE project or its authors.
	-->
	<xsl:template name="table.footnote.block">
		<xsl:if test=".//footnote">
			<fo:block keep-with-previous.within-column="always">
				<xsl:apply-templates select=".//footnote"
					mode="table.footnote.mode"/>
			</fo:block>
		</xsl:if>
		<xsl:apply-templates select="./caption"/>
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
				<fo:instream-foreign-object>
					<xsl:apply-templates/>
				</fo:instream-foreign-object>
			</xsl:when>
			<xsl:when xmlns:mml="http://www.w3.org/1998/Math/MathML"
				test="mml:*">
				<fo:instream-foreign-object>
					<xsl:apply-templates/>
				</fo:instream-foreign-object>
			</xsl:when>
			<xsl:otherwise>
				<xsl:apply-imports/>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	<!--this template is originally from the DocBook project; it has been
		modified for caption handling-->
	<xsl:template match="caption/para[count(preceding-sibling::*)=0]">
		<fo:block xsl:use-attribute-sets="normal.para.spacing">
			<xsl:call-template name="anchor"/>
			<fo:inline font-weight="bold">Caption: </fo:inline>
			<xsl:apply-templates/>
		</fo:block>
	</xsl:template>
	<!--this template is originally from the DocBook project; it has been
		modified for question handling-->
	<xsl:template match="question/para[count(preceding-sibling::*)=0]">
		<fo:block xsl:use-attribute-sets="normal.para.spacing">
			<xsl:call-template name="anchor"/>
			<fo:inline font-weight="bold">Q: </fo:inline>
			<xsl:apply-templates/>
		</fo:block>
	</xsl:template>
	<!--this template is originally from the DocBook project; it has been
		modified for answer handling-->
	<xsl:template match="answer/para[count(preceding-sibling::*)=0]">
		<fo:block xsl:use-attribute-sets="normal.para.spacing">
			<xsl:call-template name="anchor"/>
			<fo:inline font-weight="bold">A: </fo:inline>
			<xsl:apply-templates/>
		</fo:block>
	</xsl:template>
	<!--this template is originally from the DocBook project; it has been
		modified to put title content instead of titleabbrev content in the toc->
	<xsl:template name="toc.line">
		<xsl:param name="toc-context" select="NOTANODE"/>

		<xsl:variable name="id">
			<xsl:call-template name="object.id"/>
		</xsl:variable>

		<xsl:variable name="label">
			<xsl:apply-templates select="." mode="label.markup"/>
		</xsl:variable>

		<fo:block xsl:use-attribute-sets="toc.line.properties"
			end-indent="{$toc.indent.width}pt"
			last-line-end-indent="-{$toc.indent.width}pt">
			<fo:inline keep-with-next.within-line="always">
				<fo:basic-link internal-destination="{$id}">
					<xsl:if test="$label != ''">
						<xsl:copy-of select="$label"/>
						<xsl:value-of select="$autotoc.label.separator"/>
					</xsl:if>
					<!-changed to title.markup->
					<xsl:apply-templates select="." mode="title.markup"/>
				</fo:basic-link>
			</fo:inline>
			<fo:inline keep-together.within-line="always">
				<xsl:text> </xsl:text>
				<fo:leader leader-pattern="dots" leader-pattern-width="3pt"
					leader-alignment="reference-area"
					keep-with-next.within-line="always"/>
				<xsl:text> </xsl:text>
				<fo:basic-link internal-destination="{$id}">
					<fo:page-number-citation ref-id="{$id}"/>
				</fo:basic-link>
			</fo:inline>
		</fo:block>
	</xsl:template-->
</xsl:stylesheet>
<!--
	MMADE, a Mathematica DocBook Exporter
	The license and Copyright information for MMADE is included in rights.txt
	in the XML directory.
-->