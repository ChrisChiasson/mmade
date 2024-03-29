<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:xlink="http://www.w3.org/1999/xlink"
                xmlns:stext="http://nwalsh.com/xslt/ext/com.nwalsh.saxon.TextFactory"
                xmlns:simg="http://nwalsh.com/xslt/ext/com.nwalsh.saxon.ImageIntrinsics"
                xmlns:ximg="xaln://com.nwalsh.xalan.ImageIntrinsics"
                xmlns:xtext="com.nwalsh.xalan.Text"
                xmlns:lxslt="http://xml.apache.org/xslt"
                exclude-result-prefixes="xlink stext xtext lxslt simg ximg"
                extension-element-prefixes="stext xtext"
                version='1.0'>
	<!--this stylesheet is originally from the DocBook project; it has been
		modified for alignment-adjust fo processing instruction handling-->		
<xsl:template name="process.image">
  <!-- When this template is called, the current node should be  -->
  <!-- a graphic, inlinegraphic, imagedata, or videodata. All    -->
  <!-- those elements have the same set of attributes, so we can -->
  <!-- handle them all in one place.                             -->
  <xsl:param name="tag" select="'img'"/>
  <xsl:param name="alt"/>
  <xsl:param name="longdesc"/>

  <!-- The HTML img element only supports the notion of content-area
       scaling; it doesn't support the distinction between a
       content-area and a viewport-area, so we have to make some
       compromises.

       1. If only the content-area is specified, everything is fine.
          (If you ask for a three inch image, that's what you'll get.)

       2. If only the viewport-area is provided:
          - If scalefit=1, treat it as both the content-area and
            the viewport-area. (If you ask for an image in a five inch
            area, we'll make the image five inches to fill that area.)
          - If scalefit=0, ignore the viewport-area specification.

          Note: this is not quite the right semantic and has the additional
          problem that it can result in anamorphic scaling, which scalefit
          should never cause.

       3. If both the content-area and the viewport-area is specified
          on a graphic element, ignore the viewport-area.
          (If you ask for a three inch image in a five inch area, we'll assume
           it's better to give you a three inch image in an unspecified area
           than a five inch image in a five inch area.

       Relative units also cause problems. As a general rule, the stylesheets
       are operating too early and too loosely coupled with the rendering engine
       to know things like the current font size or the actual dimensions of
       an image. Therefore:

       1. We use a fixed size for pixels, $pixels.per.inch

       2. We use a fixed size for "em"s, $points.per.em

       Percentages are problematic. In the following discussion, we speak
       of width and contentwidth, but the same issues apply to depth and
       contentdepth

       1. A width of 50% means "half of the available space for the image."
          That's fine. But note that in HTML, this is a dynamic property and
          the image size will vary if the browser window is resized.

       2. A contentwidth of 50% means "half of the actual image width". But
          the stylesheets have no way to assess the image's actual size. Treating
          this as a width of 50% is one possibility, but it produces behavior
          (dynamic scaling) that seems entirely out of character with the
          meaning.

          Instead, the stylesheets define a $nominal.image.width
          and convert percentages to actual values based on that nominal size.

       Scale can be problematic. Scale applies to the contentwidth, so
       a scale of 50 when a contentwidth is not specified is analagous to a
       width of 50%. (If a contentwidth is specified, the scaling factor can
       be applied to that value and no problem exists.)

       If scale is specified but contentwidth is not supplied, the
       nominal.image.width is used to calculate a base size
       for scaling.

       Warning: as a consequence of these decisions, unless the aspect ratio
       of your image happens to be exactly the same as (nominal width / nominal height),
       specifying contentwidth="50%" and contentdepth="50%" is NOT going to
       scale the way you expect (or really, the way it should).

       Don't do that. In fact, a percentage value is not recommended for content
       size at all. Use scale instead.

       Finally, align and valign are troublesome. Horizontal alignment is now
       supported by wrapping the image in a <div align="{@align}"> (in block
       contexts!). I can't think of anything (practical) to do about vertical
       alignment.
  -->

  <xsl:variable name="width-units">
    <xsl:choose>
      <xsl:when test="$ignore.image.scaling != 0"></xsl:when>
      <xsl:when test="@width">
        <xsl:call-template name="length-units">
          <xsl:with-param name="length" select="@width"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="not(@depth) and $default.image.width != ''">
        <xsl:call-template name="length-units">
          <xsl:with-param name="length" select="$default.image.width"/>
        </xsl:call-template>
      </xsl:when>
    </xsl:choose>
  </xsl:variable>

  <xsl:variable name="width">
    <xsl:choose>
      <xsl:when test="$ignore.image.scaling != 0"></xsl:when>
      <xsl:when test="@width">
        <xsl:choose>
          <xsl:when test="$width-units = '%'">
            <xsl:value-of select="@width"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:call-template name="length-spec">
              <xsl:with-param name="length" select="@width"/>
            </xsl:call-template>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:when test="not(@depth) and $default.image.width != ''">
        <xsl:value-of select="$default.image.width"/>
      </xsl:when>
    </xsl:choose>
  </xsl:variable>

  <xsl:variable name="scalefit">
    <xsl:choose>
      <xsl:when test="$ignore.image.scaling != 0">0</xsl:when>
      <xsl:when test="@contentwidth or @contentdepth">0</xsl:when>
      <xsl:when test="@scale">0</xsl:when>
      <xsl:when test="@scalefit"><xsl:value-of select="@scalefit"/></xsl:when>
      <xsl:when test="$width != '' or @depth">1</xsl:when>
      <xsl:otherwise>0</xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <xsl:variable name="scale">
    <xsl:choose>
      <xsl:when test="$ignore.image.scaling != 0">1.0</xsl:when>
      <xsl:when test="@contentwidth or @contentdepth">1.0</xsl:when>
      <xsl:when test="@scale">
        <xsl:value-of select="@scale div 100.0"/>
      </xsl:when>
      <xsl:otherwise>1.0</xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <xsl:variable name="filename">
    <xsl:choose>
      <xsl:when test="local-name(.) = 'graphic'
                      or local-name(.) = 'inlinegraphic'">
        <!-- handle legacy graphic and inlinegraphic by new template --> 
        <xsl:call-template name="mediaobject.filename">
          <xsl:with-param name="object" select="."/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <!-- imagedata, videodata, audiodata -->
        <xsl:call-template name="mediaobject.filename">
          <xsl:with-param name="object" select=".."/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <xsl:variable name="output_filename">
    <xsl:choose>
      <xsl:when test="@entityref">
	<xsl:value-of select="$filename"/>
      </xsl:when>
      <!--
        Moved test for $keep.relative.image.uris to template below:
            <xsl:template match="@fileref">
      -->
      <xsl:otherwise>
	<xsl:value-of select="$filename"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <xsl:variable name="img.src.path.pi">
    <xsl:call-template name="dbhtml-attribute">
      <xsl:with-param name="pis"
                      select="../processing-instruction('dbhtml')"/>
      <xsl:with-param name="attribute" select="'img.src.path'"/>
    </xsl:call-template>
  </xsl:variable>

  <xsl:variable name="filename.for.graphicsize">
    <xsl:choose>
      <xsl:when test="$img.src.path.pi != ''">
        <xsl:value-of select="concat($img.src.path.pi, $filename)"/>
      </xsl:when>
      <xsl:when test="$img.src.path != '' and
                      $graphicsize.use.img.src.path != 0 and
                      $tag = 'img' and
                      not(starts-with($filename, '/')) and
                      not(contains($filename, '://'))">
        <xsl:value-of select="concat($img.src.path, $filename)"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$filename"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <xsl:variable name="realintrinsicwidth">
    <!-- This funny compound test works around a bug in XSLTC -->
    <xsl:choose>
      <xsl:when test="$use.extensions != 0 and $graphicsize.extension != 0">
        <xsl:choose>
          <xsl:when test="function-available('simg:getWidth')">
            <xsl:value-of select="simg:getWidth(simg:new($filename.for.graphicsize),
                                                $nominal.image.width)"/>
          </xsl:when>
          <xsl:when test="function-available('ximg:getWidth')">
            <xsl:value-of select="ximg:getWidth(ximg:new($filename.for.graphicsize),
                                                $nominal.image.width)"/>
          </xsl:when>
          <xsl:otherwise>
           <xsl:value-of select="0"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="0"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <xsl:variable name="intrinsicwidth">
    <xsl:choose>
      <xsl:when test="$realintrinsicwidth = 0">
       <xsl:value-of select="$nominal.image.width"/>
      </xsl:when>
      <xsl:otherwise>
       <xsl:value-of select="$realintrinsicwidth"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <xsl:variable name="intrinsicdepth">
    <!-- This funny compound test works around a bug in XSLTC -->
    <xsl:choose>
      <xsl:when test="$use.extensions != 0 and $graphicsize.extension != 0">
        <xsl:choose>
          <xsl:when test="function-available('simg:getDepth')">
            <xsl:value-of select="simg:getDepth(simg:new($filename.for.graphicsize),
                                                $nominal.image.depth)"/>
          </xsl:when>
          <xsl:when test="function-available('ximg:getDepth')">
            <xsl:value-of select="ximg:getDepth(ximg:new($filename.for.graphicsize),
                                                $nominal.image.depth)"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="$nominal.image.depth"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$nominal.image.depth"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <xsl:variable name="contentwidth">
    <xsl:choose>
      <xsl:when test="$ignore.image.scaling != 0"></xsl:when>
      <xsl:when test="@contentwidth">
        <xsl:variable name="units">
          <xsl:call-template name="length-units">
            <xsl:with-param name="length" select="@contentwidth"/>
          </xsl:call-template>
        </xsl:variable>

        <xsl:choose>
          <xsl:when test="$units = '%'">
            <xsl:variable name="cmagnitude">
              <xsl:call-template name="length-magnitude">
                <xsl:with-param name="length" select="@contentwidth"/>
              </xsl:call-template>
            </xsl:variable>
            <xsl:value-of select="$intrinsicwidth * $cmagnitude div 100.0"/>
            <xsl:text>px</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:call-template name="length-spec">
              <xsl:with-param name="length" select="@contentwidth"/>
            </xsl:call-template>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$intrinsicwidth"/>
        <xsl:text>px</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <xsl:variable name="scaled.contentwidth">
    <xsl:if test="$contentwidth != ''">
      <xsl:variable name="cwidth.in.points">
        <xsl:call-template name="length-in-points">
          <xsl:with-param name="length" select="$contentwidth"/>
          <xsl:with-param name="pixels.per.inch" select="$pixels.per.inch"/>
          <xsl:with-param name="em.size" select="$points.per.em"/>
        </xsl:call-template>
      </xsl:variable>
      <xsl:value-of select="$cwidth.in.points div 72.0 * $pixels.per.inch * $scale"/>
    </xsl:if>
  </xsl:variable>

  <xsl:variable name="html.width">
    <xsl:choose>
      <xsl:when test="$ignore.image.scaling != 0"></xsl:when>
      <xsl:when test="$width-units = '%'">
        <xsl:value-of select="$width"/>
      </xsl:when>
      <xsl:when test="$width != ''">
        <xsl:variable name="width.in.points">
          <xsl:call-template name="length-in-points">
            <xsl:with-param name="length" select="$width"/>
            <xsl:with-param name="pixels.per.inch" select="$pixels.per.inch"/>
            <xsl:with-param name="em.size" select="$points.per.em"/>
          </xsl:call-template>
        </xsl:variable>
        <xsl:value-of select="round($width.in.points div 72.0 * $pixels.per.inch)"/>
      </xsl:when>
      <xsl:otherwise></xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <xsl:variable name="contentdepth">
    <xsl:choose>
      <xsl:when test="$ignore.image.scaling != 0"></xsl:when>
      <xsl:when test="@contentdepth">
        <xsl:variable name="units">
          <xsl:call-template name="length-units">
            <xsl:with-param name="length" select="@contentdepth"/>
          </xsl:call-template>
        </xsl:variable>

        <xsl:choose>
          <xsl:when test="$units = '%'">
            <xsl:variable name="cmagnitude">
              <xsl:call-template name="length-magnitude">
                <xsl:with-param name="length" select="@contentdepth"/>
              </xsl:call-template>
            </xsl:variable>
            <xsl:value-of select="$intrinsicdepth * $cmagnitude div 100.0"/>
            <xsl:text>px</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:call-template name="length-spec">
              <xsl:with-param name="length" select="@contentdepth"/>
            </xsl:call-template>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$intrinsicdepth"/>
        <xsl:text>px</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <xsl:variable name="scaled.contentdepth">
    <xsl:if test="$contentdepth != ''">
      <xsl:variable name="cdepth.in.points">
        <xsl:call-template name="length-in-points">
          <xsl:with-param name="length" select="$contentdepth"/>
          <xsl:with-param name="pixels.per.inch" select="$pixels.per.inch"/>
          <xsl:with-param name="em.size" select="$points.per.em"/>
        </xsl:call-template>
      </xsl:variable>
      <xsl:value-of select="$cdepth.in.points div 72.0 * $pixels.per.inch * $scale"/>
    </xsl:if>
  </xsl:variable>

  <xsl:variable name="depth-units">
    <xsl:if test="@depth">
      <xsl:call-template name="length-units">
        <xsl:with-param name="length" select="@depth"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:variable>

  <xsl:variable name="depth">
    <xsl:if test="@depth">
      <xsl:choose>
        <xsl:when test="$depth-units = '%'">
          <xsl:value-of select="@depth"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:call-template name="length-spec">
            <xsl:with-param name="length" select="@depth"/>
          </xsl:call-template>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:if>
  </xsl:variable>

  <xsl:variable name="html.depth">
    <xsl:choose>
      <xsl:when test="$ignore.image.scaling != 0"></xsl:when>
      <xsl:when test="$depth-units = '%'">
        <xsl:value-of select="$depth"/>
      </xsl:when>
      <xsl:when test="@depth and @depth != ''">
        <xsl:variable name="depth.in.points">
          <xsl:call-template name="length-in-points">
            <xsl:with-param name="length" select="$depth"/>
            <xsl:with-param name="pixels.per.inch" select="$pixels.per.inch"/>
            <xsl:with-param name="em.size" select="$points.per.em"/>
          </xsl:call-template>
        </xsl:variable>
        <xsl:value-of select="round($depth.in.points div 72.0 * $pixels.per.inch)"/>
      </xsl:when>
      <xsl:otherwise></xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <xsl:variable name="viewport">
    <xsl:choose>
      <xsl:when test="$ignore.image.scaling != 0">0</xsl:when>
      <xsl:when test="local-name(.) = 'inlinegraphic'
                      or ancestor::inlinemediaobject
                      or ancestor::inlineequation">0</xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$make.graphic.viewport"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

<!--
  <xsl:message>=====================================
scale: <xsl:value-of select="$scale"/>, <xsl:value-of select="$scalefit"/>
@contentwidth <xsl:value-of select="@contentwidth"/>
$contentwidth <xsl:value-of select="$contentwidth"/>
scaled.contentwidth: <xsl:value-of select="$scaled.contentwidth"/>
@width: <xsl:value-of select="@width"/>
width: <xsl:value-of select="$width"/>
html.width: <xsl:value-of select="$html.width"/>
@contentdepth <xsl:value-of select="@contentdepth"/>
$contentdepth <xsl:value-of select="$contentdepth"/>
scaled.contentdepth: <xsl:value-of select="$scaled.contentdepth"/>
@depth: <xsl:value-of select="@depth"/>
depth: <xsl:value-of select="$depth"/>
html.depth: <xsl:value-of select="$html.depth"/>
align: <xsl:value-of select="@align"/>
valign: <xsl:value-of select="@valign"/></xsl:message>
-->

  <xsl:variable name="scaled"
              select="@width|@depth|@contentwidth|@contentdepth
                        |@scale|@scalefit"/>

	<!--Chris Chiasson added the following variable and processing instruction
		call for the mmade project-->	
	<xsl:variable name="alignment-adjust">
		<xsl:call-template name="dbhtml-attribute">
			<xsl:with-param name="pis"
				select="ancestor-or-self::imagedata/processing-instruction('dbhtml')"/>
			<xsl:with-param name="attribute" select="'alignment-adjust'"/>
		</xsl:call-template>
	</xsl:variable>

	<xsl:variable name="img">
    <xsl:choose>
      <xsl:when test="@format = 'SVG'">
        <object data="{$output_filename}" type="image/svg+xml">
          <xsl:call-template name="process.image.attributes">
            <!--xsl:with-param name="alt" select="$alt"/ there's no alt here-->
            <xsl:with-param name="html.depth" select="$html.depth"/>
            <xsl:with-param name="html.width" select="$html.width"/>
            <xsl:with-param name="longdesc" select="$longdesc"/>
            <xsl:with-param name="scale" select="$scale"/>
            <xsl:with-param name="scalefit" select="$scalefit"/>
            <xsl:with-param name="scaled.contentdepth" select="$scaled.contentdepth"/>
            <xsl:with-param name="scaled.contentwidth" select="$scaled.contentwidth"/>
            <xsl:with-param name="viewport" select="$viewport"/>
          </xsl:call-template>
          <xsl:if test="@align">
            <xsl:attribute name="align">
                <xsl:choose>
                  <xsl:when test="@align = 'center'">middle</xsl:when>
                  <xsl:otherwise>
                    <xsl:value-of select="@align"/>
                  </xsl:otherwise>
                </xsl:choose>
            </xsl:attribute>
          </xsl:if>
          <xsl:if test="$use.embed.for.svg != 0">
            <embed src="{$output_filename}" type="image/svg+xml">
              <xsl:call-template name="process.image.attributes">
                <!--xsl:with-param name="alt" select="$alt"/ there's no alt here -->
                <xsl:with-param name="html.depth" select="$html.depth"/>
                <xsl:with-param name="html.width" select="$html.width"/>
                <xsl:with-param name="longdesc" select="$longdesc"/>
                <xsl:with-param name="scale" select="$scale"/>
                <xsl:with-param name="scalefit" select="$scalefit"/>
                <xsl:with-param name="scaled.contentdepth" select="$scaled.contentdepth"/>
                <xsl:with-param name="scaled.contentwidth" select="$scaled.contentwidth"/>
                <xsl:with-param name="viewport" select="$viewport"/>
              </xsl:call-template>
            </embed>
          </xsl:if>
        </object>
      </xsl:when>
      <xsl:otherwise>
        <xsl:element name="{$tag}">
         <xsl:if test="$tag = 'img' and ../../self::imageobjectco">
           <xsl:choose>
             <xsl:when test="$scaled">
              <!-- It might be possible to handle some scaling; needs -->
              <!-- more investigation -->
              <xsl:message>
                <xsl:text>Warning: imagemaps not supported </xsl:text>
                <xsl:text>on scaled images</xsl:text>
              </xsl:message>
             </xsl:when>
             <xsl:otherwise>
              <xsl:attribute name="border">0</xsl:attribute>
              <xsl:attribute name="usemap">
                <xsl:value-of select="generate-id(../..)"/>
              </xsl:attribute>
             </xsl:otherwise>
           </xsl:choose>
         </xsl:if>

        	<!--Chris Chiasson added the following if test and attribute for the
        		mmade project-->
        	<xsl:if test="$alignment-adjust != ''">
        		<xsl:attribute name="style">
        			<xsl:text>vertical-align: </xsl:text>
        			<xsl:value-of select="$alignment-adjust"/>
        		</xsl:attribute>
        	</xsl:if>          	
        	
          <xsl:attribute name="src">
           <xsl:choose>
             <xsl:when test="$img.src.path != '' and
                           $tag = 'img' and
                             not(starts-with($output_filename, '/')) and
                           not(contains($output_filename, '://'))">
               <xsl:value-of select="$img.src.path"/>
             </xsl:when>
           </xsl:choose>
            <xsl:value-of select="$output_filename"/>
          </xsl:attribute>

          <xsl:if test="@align">
            <xsl:attribute name="align">
              <xsl:choose>
                <xsl:when test="@align = 'center'">middle</xsl:when>
                <xsl:otherwise>
                  <xsl:value-of select="@align"/>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:attribute>
          </xsl:if>

          <xsl:call-template name="process.image.attributes">
            <xsl:with-param name="alt">
              <xsl:choose>
                <xsl:when test="$alt != ''">
                  <xsl:copy-of select="$alt"/>
                </xsl:when>
                <xsl:when test="ancestor::figure">
                  <xsl:value-of select="normalize-space(ancestor::figure/title)"/>
                </xsl:when>
              </xsl:choose>
            </xsl:with-param>
            <xsl:with-param name="html.depth" select="$html.depth"/>
            <xsl:with-param name="html.width" select="$html.width"/>
            <xsl:with-param name="longdesc" select="$longdesc"/>
            <xsl:with-param name="scale" select="$scale"/>
            <xsl:with-param name="scalefit" select="$scalefit"/>
            <xsl:with-param name="scaled.contentdepth" select="$scaled.contentdepth"/>
            <xsl:with-param name="scaled.contentwidth" select="$scaled.contentwidth"/>
            <xsl:with-param name="viewport" select="$viewport"/>
          </xsl:call-template>
        </xsl:element>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <xsl:variable name="bgcolor">
    <xsl:call-template name="dbhtml-attribute">
      <xsl:with-param name="pis"
                      select="../processing-instruction('dbhtml')"/>
      <xsl:with-param name="attribute" select="'background-color'"/>
    </xsl:call-template>
  </xsl:variable>
	
	<xsl:variable name="use.viewport"
                select="$viewport != 0
                        and ($html.width != ''
                             or ($html.depth != '' and $depth-units != '%')
                             or $bgcolor != ''
                             or @valign)"/>

  <xsl:choose>
    <xsl:when test="$use.viewport">
      <table border="0" summary="manufactured viewport for HTML img"
             cellspacing="0" cellpadding="0">
        <xsl:if test="$html.width != ''">
          <xsl:attribute name="width">
            <xsl:value-of select="$html.width"/>
          </xsl:attribute>
        </xsl:if>
        <tr>
          <xsl:if test="$html.depth != '' and $depth-units != '%'">
            <!-- don't do this for percentages because browsers get confused -->
            <xsl:choose>
              <xsl:when test="$css.decoration != 0">
                <xsl:attribute name="style">
                  <xsl:text>height: </xsl:text>
                  <xsl:value-of select="$html.depth"/>
                  <xsl:text>px</xsl:text>
                </xsl:attribute>
              </xsl:when>
              <xsl:otherwise>
                <xsl:attribute name="height">
                  <xsl:value-of select="$html.depth"/>
                </xsl:attribute>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:if>
          <td>
            <xsl:if test="$bgcolor != ''">
              <xsl:choose>
                <xsl:when test="$css.decoration != 0">
                  <xsl:attribute name="style">
                    <xsl:text>background-color: </xsl:text>
                    <xsl:value-of select="$bgcolor"/>
                  </xsl:attribute>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:attribute name="bgcolor">
                    <xsl:value-of select="$bgcolor"/>
                  </xsl:attribute>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:if>
            <xsl:if test="@align">
              <xsl:attribute name="align">
                <xsl:value-of select="@align"/>
              </xsl:attribute>
            </xsl:if>
            <xsl:if test="@valign">
              <xsl:attribute name="valign">
                <xsl:value-of select="@valign"/>
              </xsl:attribute>
            </xsl:if>
            <xsl:copy-of select="$img"/>
          </td>
        </tr>
      </table>
    </xsl:when>
    <xsl:otherwise>
      <xsl:copy-of select="$img"/>
    </xsl:otherwise>
  </xsl:choose>

  <xsl:if test="$tag = 'img' and ../../self::imageobjectco and not($scaled)">
    <map name="{generate-id(../..)}">
      <xsl:for-each select="../../areaspec//area">
       <xsl:variable name="units">
         <xsl:choose>
           <xsl:when test="@units">
             <xsl:value-of select="@units"/>
           </xsl:when>
           <xsl:when test="../@units">
             <xsl:value-of select="../@units"/>
           </xsl:when>
           <xsl:otherwise>calspair</xsl:otherwise>
         </xsl:choose>
       </xsl:variable>

       <xsl:choose>
         <xsl:when test="$units = 'calspair'">
           <xsl:variable name="coords" select="normalize-space(@coords)"/>
           <xsl:variable name="p1"
                       select="substring-before($coords, ' ')"/>
           <xsl:variable name="p2"
                       select="substring-after($coords, ' ')"/>

           <xsl:variable name="x1" select="substring-before($p1,',')"/>
           <xsl:variable name="y1" select="substring-after($p1,',')"/>
           <xsl:variable name="x2" select="substring-before($p2,',')"/>
           <xsl:variable name="y2" select="substring-after($p2,',')"/>

           <xsl:variable name="x1p" select="$x1 div 100.0"/>
           <xsl:variable name="y1p" select="$y1 div 100.0"/>
           <xsl:variable name="x2p" select="$x2 div 100.0"/>
	   <xsl:variable name="y2p" select="$y2 div 100.0"/>

<!--
           <xsl:message>
             <xsl:text>units: </xsl:text>
             <xsl:value-of select="$units"/>
             <xsl:text> </xsl:text>
             <xsl:value-of select="$x1p"/><xsl:text>, </xsl:text>
             <xsl:value-of select="$y1p"/><xsl:text>, </xsl:text>
             <xsl:value-of select="$x2p"/><xsl:text>, </xsl:text>
             <xsl:value-of select="$y2p"/><xsl:text>, </xsl:text>
           </xsl:message>

           <xsl:message>
             <xsl:text>      </xsl:text>
             <xsl:value-of select="$intrinsicwidth"/>
             <xsl:text>, </xsl:text>
             <xsl:value-of select="$intrinsicdepth"/>
           </xsl:message>

           <xsl:message>
             <xsl:text>      </xsl:text>
             <xsl:value-of select="$units"/>
             <xsl:text> </xsl:text>
             <xsl:value-of select="round($x1p * $intrinsicwidth div 100.0)"/>
             <xsl:text>,</xsl:text>
             <xsl:value-of select="round($intrinsicdepth
                                - ($y1p * $intrinsicdepth div 100.0))"/>
             <xsl:text>,</xsl:text>
             <xsl:value-of select="round($x2p * $intrinsicwidth div 100.0)"/>
             <xsl:text>,</xsl:text>
             <xsl:value-of select="round($intrinsicdepth
                                - ($y2p * $intrinsicdepth div 100.0))"/>
           </xsl:message>
-->

	   <area shape="rect">
	     <xsl:variable name="linkends">
	       <xsl:choose>
		 <xsl:when test="@linkends">
		   <xsl:value-of select="normalize-space(@linkends)"/>
		 </xsl:when>
		 <xsl:otherwise>
		   <xsl:value-of select="normalize-space(../@linkends)"/>
		 </xsl:otherwise>
	       </xsl:choose>
	     </xsl:variable>

	     <xsl:variable name="href">
	       <xsl:choose>
		 <xsl:when test="@xlink:href">
		   <xsl:value-of select="@xlink:href"/>
		 </xsl:when>
		 <xsl:otherwise>
		   <xsl:value-of select="../@xlink:href"/>
		 </xsl:otherwise>
	       </xsl:choose>
	     </xsl:variable>

	     <xsl:choose>
	       <xsl:when test="$linkends != ''">
		 <xsl:variable name="linkend">
		   <xsl:choose>
		     <xsl:when test="contains($linkends, ' ')">
		       <xsl:value-of select="substring-before($linkends, ' ')"/>
		     </xsl:when>
		     <xsl:otherwise>
		       <xsl:value-of select="$linkends"/>
		     </xsl:otherwise>
		   </xsl:choose>
		 </xsl:variable>
		 
		 <xsl:variable name="target" select="key('id', $linkend)[1]"/>
		
		 <xsl:if test="$target">
		   <xsl:attribute name="href">
		     <xsl:call-template name="href.target">
		       <xsl:with-param name="object" select="$target"/>
		     </xsl:call-template>
		   </xsl:attribute>
		 </xsl:if>
	       </xsl:when>
	       <xsl:when test="$href != ''">
		 <xsl:attribute name="href">
		   <xsl:value-of select="$href"/>
		 </xsl:attribute>
	       </xsl:when>
	     </xsl:choose>

	     <xsl:if test="alt">
	       <xsl:attribute name="alt">
		 <xsl:value-of select="alt[1]"/>
	       </xsl:attribute>
	     </xsl:if>

	     <xsl:attribute name="coords">
	       <xsl:value-of select="round($x1p * $intrinsicwidth div 100.0)"/>
	       <xsl:text>,</xsl:text>
	       <xsl:value-of select="round($intrinsicdepth
				       - ($y1p * $intrinsicdepth div 100.0))"/>
	       <xsl:text>,</xsl:text>
	       <xsl:value-of select="round($x2p * $intrinsicwidth div 100.0)"/>
	       <xsl:text>,</xsl:text>
	       <xsl:value-of select="round($intrinsicdepth
				     - ($y2p * $intrinsicdepth div 100.0))"/>
	     </xsl:attribute>
	   </area>
         </xsl:when>
         <xsl:otherwise>
           <xsl:message>
             <xsl:text>Warning: only calspair supported </xsl:text>
             <xsl:text>in imageobjectco</xsl:text>
           </xsl:message>
         </xsl:otherwise>
       </xsl:choose>
      </xsl:for-each>
    </map>
  </xsl:if>
</xsl:template>
</xsl:stylesheet>