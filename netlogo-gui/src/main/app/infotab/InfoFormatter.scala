// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.app.infotab

import java.io.InputStream

import org.pegdown.{ Extensions, PegDownProcessor }

import org.nlogo.api.FileIO

// This gets tested by TestInfoFormatter. - ST 9/7/10

object InfoFormatter {

  type MarkDownString = String
  type HTML = String
  type CSS = String

  val MaxParsingTimeMillis = 4000 // set high for Travis, won't take that long on most computers

  val pegDown = new PegDownProcessor(Extensions.SMARTYPANTS |       // beautifies quotes, dashes, etc.
                                     Extensions.AUTOLINKS |         // angle brackets around URLs and email addresses not needed
                                     Extensions.HARDWRAPS |         // GitHub flavored newlines
                                     Extensions.FENCED_CODE_BLOCKS, // delimit code blocks with ```
                                     MaxParsingTimeMillis)

  /**
   * for standalone use, for example on a web server
   */
  def main(argv: Array[String]) {
    println(apply(read(System.in)))
  }
  def read(in: InputStream): String = io.Source.fromInputStream(in).mkString

  def styleSheetFile: CSS = FileIO.getResourceAsString("/system/info.css")
  val defaultFontSize = 14
  val defaultStyleSheet: CSS = styleSheet(defaultFontSize)
  def styleSheet(fontSize: Int): CSS = "<style type=\"text/css\">\n<!--\n"+
          styleSheetFile.
            replace("{BODY-FONT-SIZE}", fontSize.toString).
            replace("{H1-FONT-SIZE}", (fontSize * 1.5).toInt.toString).
            replace("{H2-FONT-SIZE}", (fontSize * 1.25).toInt.toString).
            replace("{H3-FONT-SIZE}", fontSize.toString).
            replace("{BULLET-IMAGE}", getClass.getResource("/system/bullet.png").toString) + "\n-->\n</style>"

  def toInnerHtml(str: MarkDownString): HTML = pegDown.markdownToHtml(str)

  def wrapHtml(body: HTML, fontSize: Int = defaultFontSize): HTML =
    "<html><head>"+styleSheet(fontSize)+"</head><body>"+body+"</body></html>"

  def apply(content: String, fontSize: Int = defaultFontSize) =
    wrapHtml(toInnerHtml(content), fontSize)

}
