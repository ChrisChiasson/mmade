<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns:fo="http://www.w3.org/1999/XSL/Format"
	xmlns:psmi="http://www.CraneSoftwrights.com/resources/psmi"
	exclude-result-prefixes="psmi" version="1.0">


	<xsl:import
		href="http://docbook.sourceforge.net/release/xsl/current/fo/docbook.xsl"/>

	<xsl:param name="double.sided" select="'1'"/>

	<xsl:template name="user.pagemasters">
		<fo:simple-page-master master-name="body-landscape-odd"
			page-width="{$page.width}" page-height="{$page.height}"
			margin-top="{$page.margin.top}"
			margin-bottom="{$page.margin.bottom}"
			margin-left="{$page.margin.inner}"
			margin-right="{$page.margin.outer}">
			<fo:region-body region-name="region-body-landscape"
				margin-bottom="1in" reference-orientation="90deg"
				margin-top="1in" column-count="{$column.count.body}"> </fo:region-body>
			<fo:region-before region-name="xsl-region-before-odd"
				extent="{$region.before.extent}" display-align="before"/>
			<fo:region-after region-name="xsl-region-after-odd"
				extent="{$region.after.extent}" display-align="after"/>
		</fo:simple-page-master>
		<fo:simple-page-master master-name="body-landscape-even"
			page-width="{$page.width}" page-height="{$page.height}"
			margin-top="{$page.margin.top}"
			margin-bottom="{$page.margin.bottom}"
			margin-left="{$page.margin.inner}"
			margin-right="{$page.margin.outer}">
			<fo:region-body region-name="region-body-landscape"
				margin-bottom="1in" reference-orientation="90deg"
				margin-top="1in" column-count="{$column.count.body}"> </fo:region-body>
			<fo:region-before region-name="xsl-region-before-even"
				extent="{$region.before.extent}" display-align="before"/>
			<fo:region-after region-name="xsl-region-after-even"
				extent="{$region.after.extent}" display-align="after"/>
		</fo:simple-page-master>

		<!-- setup for body pages -->
		<fo:page-sequence-master master-name="body-landscape">
			<fo:repeatable-page-master-alternatives>
				<fo:conditional-page-master-reference
					master-reference="body-landscape-odd" odd-or-even="odd"/>
				<fo:conditional-page-master-reference
					master-reference="body-landscape-even" odd-or-even="even"/>
			</fo:repeatable-page-master-alternatives>
		</fo:page-sequence-master>



	</xsl:template>


	<xsl:template match="chapter[@role = 'land']">
		<xsl:variable name="id">
			<xsl:call-template name="object.id"/>
		</xsl:variable>

		<xsl:variable name="master-reference">
			<xsl:call-template name="select.pagemaster"/>
		</xsl:variable>

		<fo:page-sequence id="{$id}" hyphenate="{$hyphenate}"
			master-reference="{$master-reference}">
			<xsl:attribute name="language">
				<xsl:call-template name="l10n.language"/>
			</xsl:attribute>
			<xsl:attribute name="format">
				<xsl:call-template name="page.number.format"/>
			</xsl:attribute>
			<xsl:choose>
				<xsl:when
					test="not(preceding::chapter
                          or preceding::appendix
                          or preceding::article
                          or preceding::dedication
                          or parent::part
                          or parent::reference)">
				<!-- if there is a preceding component or we're in a part, the -->
				<!-- page numbering will already be adjusted -->
					<xsl:attribute name="initial-page-number">1</xsl:attribute>
				</xsl:when>
				<xsl:when test="$double.sided != 0">
					<xsl:attribute name="initial-page-number"
					>auto-odd</xsl:attribute>
				</xsl:when>
			</xsl:choose>

			<xsl:apply-templates select="." mode="running.head.mode">
				<xsl:with-param name="master-reference"
					select="$master-reference"/>
			</xsl:apply-templates>

			<xsl:apply-templates select="." mode="running.foot.mode">
				<xsl:with-param name="master-reference"
					select="$master-reference"/>
			</xsl:apply-templates>

			<fo:flow flow-name="xsl-region-body">
				<!-- psmi stuff -->
				<psmi:page-sequence master-reference="body-landscape">
					<fo:flow flow-name="region-body-landscape">
				<!-- psmi stuff -->
						<xsl:call-template name="chapter.titlepage"/>

						<xsl:variable name="toc.params">
							<xsl:call-template name="find.path.params">
								<xsl:with-param name="table"
									select="normalize-space($generate.toc)"/>
							</xsl:call-template>
						</xsl:variable>
						<xsl:if test="contains($toc.params, 'toc')">
							<xsl:call-template name="component.toc"/>
							<xsl:call-template name="component.toc.separator"/>
						</xsl:if>
						<xsl:apply-templates/>
				<!-- psmi stuff -->
					</fo:flow>
				</psmi:page-sequence>
				<!-- psmi stuff -->
			</fo:flow>
		</fo:page-sequence>
	</xsl:template>


	<xsl:template match="appendix[@role = 'land']">
		<xsl:variable name="id">
			<xsl:call-template name="object.id"/>
		</xsl:variable>

		<xsl:variable name="master-reference">
			<xsl:call-template name="select.pagemaster"/>
		</xsl:variable>

		<fo:page-sequence id="{$id}" hyphenate="{$hyphenate}"
			master-reference="{$master-reference}">
			<xsl:attribute name="language">
				<xsl:call-template name="l10n.language"/>
			</xsl:attribute>
			<xsl:attribute name="format">
				<xsl:call-template name="page.number.format"/>
			</xsl:attribute>
			<xsl:choose>
				<xsl:when
					test="not(preceding::chapter
                          or preceding::appendix
                          or preceding::article
                          or preceding::dedication
                          or parent::part
                          or parent::reference)">
					<!-- if there is a preceding component or we're in a part, the -->
					<!-- page numbering will already be adjusted -->
					<xsl:attribute name="initial-page-number">1</xsl:attribute>
				</xsl:when>
				<xsl:when test="$double.sided != 0">
					<xsl:attribute name="initial-page-number"
					>auto-odd</xsl:attribute>
				</xsl:when>
			</xsl:choose>

			<xsl:apply-templates select="." mode="running.head.mode">
				<xsl:with-param name="master-reference"
					select="$master-reference"/>
			</xsl:apply-templates>

			<xsl:apply-templates select="." mode="running.foot.mode">
				<xsl:with-param name="master-reference"
					select="$master-reference"/>
			</xsl:apply-templates>

			<fo:flow flow-name="xsl-region-body">
				<!-- psmi stuff -->
				<psmi:page-sequence master-reference="body-landscape">
					<fo:flow flow-name="region-body-landscape">
				<!-- psmi stuff -->
						<xsl:call-template name="appendix.titlepage"/>

						<xsl:variable name="toc.params">
							<xsl:call-template name="find.path.params">
								<xsl:with-param name="table"
									select="normalize-space($generate.toc)"/>
							</xsl:call-template>
						</xsl:variable>

						<xsl:if test="contains($toc.params, 'toc')">
							<xsl:call-template name="component.toc"/>
							<xsl:call-template name="component.toc.separator"/>
						</xsl:if>
						<xsl:apply-templates/>
				<!-- psmi stuff -->
					</fo:flow>
				</psmi:page-sequence>
				<!-- psmi stuff -->
			</fo:flow>
		</fo:page-sequence>
	</xsl:template>


</xsl:stylesheet>
<!--
	This file is taken directly from
	http://sourceware.org/ml/docbook-apps/2003-q4/msg00727.html
	It is an implementation for rotating a section of an fo document into
	landscape mode through the use of the PSMI package.
-->