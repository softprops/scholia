package scholia

import scala.tools.nsc.{ast, plugins, symtab, util, Global, Phase}
import ast.parser.Tokens
import plugins.{Plugin, PluginComponent}
import symtab.Flags
import util.SourceFile

import java.io.{File, Reader, Writer}
import java.net.URL
import forScope._

object T {

  val CSSRelativePath = "scholia.css"
  val CSSShCoreRelativePath = "shThemeDefault.css"
  val JSShCoreRelativePath = "shCore.js"
  val CSSShThemeRelativePath = "shThemeEclipse.css"
  val JSShBrushRelativePath = "shBrushScala.js"

  val BaseCss = "/" + CSSRelativePath
  val ShCoreJs = "/" + JSShCoreRelativePath
  val ShCoreCss = "/" + CSSShCoreRelativePath
  val ShThemeCss = "/" + CSSShThemeRelativePath
  val ShBrushJs = "/" + JSShBrushRelativePath

  def resourced(alias: String)(rsrc: File) =
     IO.writeResource(alias, rsrc)

  val List(writeBaseCss, writeShCoreJs, writeShCoreCss, writeShThemeCss, writeShBrushJs) =
    (BaseCss :: ShCoreJs :: ShCoreCss :: ShThemeCss :: ShBrushJs :: Nil).map(resourced(_)_)

}

case class T(outputDirectory: File) {
  import T._

  val baseCss = new File(outputDirectory, CSSRelativePath)
	val shCoreJs = new File(outputDirectory, JSShCoreRelativePath)
	val shCoreCss = new File(outputDirectory, CSSShCoreRelativePath)
  val shThemeCss = new File(outputDirectory, CSSShThemeRelativePath)
  val shBrushJs = new File(outputDirectory, JSShBrushRelativePath)

  def writeResources() = {
    writeBaseCss(baseCss)
    writeShCoreJs(shCoreJs)
    writeShCoreCss(shCoreCss)
    writeShThemeCss(shThemeCss)
    writeShBrushJs(shBrushJs)
    this
  }

  def head(title: String, desc: String, of: File) = {
    def relative(f: File) = IO.relativePath(of, f)
    ("""<!DOCTYPE html>
       |<html>
       |  <head>
       |   <title>"""+title+"""</title>
       |   <link href="""+'"'+relative(shCoreCss)+'"'+""" rel="stylesheet" type="text/css" />
       |   <link href="""+'"'+relative(shThemeCss)+'"'+"""  rel="stylesheet" type="text/css" />
       |   <link href="""+'"'+relative(baseCss)+'"'+"""  rel="stylesheet" type="text/css"/>
       |   <script src="""+'"'+relative(shCoreJs)+'"'+"""  type="text/javascript"></script>
       |   <script src="""+'"'+relative(shBrushJs)+'"'+""" type="text/javascript"></script>
       |</head>
       |<body>
       |  <table>
       |   <tbody>
       |    <tr>
       |      <th class="docs"> <h1>"""+title+"""</h1><p>"""+desc+"""</p></th>
       |      <th class="code"></th>
       |    </tr>""").stripMargin
  }

  def tail =
    """|  </tbody>
       |   <script type="text/javascript">
       |    SyntaxHighlighter.defaults['gutter'] = false;
       |    SyntaxHighlighter.defaults['smart-tabs'] = false;
       |    SyntaxHighlighter.defaults['toolbar'] = false;
       |    SyntaxHighlighter.defaults['tab-size'] = 2;
       |    SyntaxHighlighter.defaults['quick-code'] = false;
       |    SyntaxHighlighter.all()
       |   </script>
       |  </body>
       |</html>""".stripMargin

}

// Comments extracted from source code
case class Comments(start: Int, length: Int) extends NotNull with Comparable[Comments] {
  def compareTo(other: Comments) = start compare other.start
}

/** The compiler plugin */
abstract class Doc extends Plugin {
  import com.tristanhunt.knockoff.DefaultDiscounter._

  def classDirectory: File

	def getRelativeSourcePath(source: File): String

  def projectName: String

  def projectDesc: String

	val global: Global
  import global._

  lazy val outputDirectory = new File(classDirectory.getParentFile, classDirectory.getName + "-lit")
	outputDirectory.mkdirs

  // Performs extraction of comments and application to template generator
	def generateOutput() {

		val sourceFiles = currentRun.units.toList.flatMap(getSourceFile(_))

		if (sourceFiles.size > 0) {

      val index = new File(outputDirectory, "index.html")

      IO.withWriter(index) { out =>

        val t = T(outputDirectory).writeResources()

        out.write(t.head(projectName, projectDesc, index))

			  for(unit <- currentRun.units; sourceFile <- getSourceFile(unit)) {

          IO.withReader(sourceFile, "utf8") { in =>
            println("processing %s" format sourceFile)
				    val comments = scan(unit)
            val relsrc = getRelativeSourcePath(sourceFile)
            val relid = relsrc.toLowerCase.replace(".scala","").replace("/","-")
            out.write("""<tr id="""+'"'+relid+'"'+""">""")
            out.write("""<td class="docs"><h2>"""+relsrc+"""</h2></td>""")
            out.write("""<td class="code"></td></tr>""")
            out.write("""<tr><td class="docs"></dt>""")

            def next(i: Int, comments: List[Comments]) {

              def escape(s: String) =
			          s.replace(">", "&gt;")
			           .replace("&", "&amp;")
			           .replace("<", "&lt;")
			           .replace("\"","&quot;")

               def consume[A](chars: Int)(include: Char => Boolean)(after: String => A) = {
                 // Buffer.empty[Byte] in 2.8
                 val buffer = new collection.mutable.ListBuffer[Byte]()
                 def take(chars: Int) {
                   if(chars > 0) {
                     val c = in.read()
                     if(c >= 0) {
                       val char = c.asInstanceOf[Char]
                       if(include(char)) buffer.append(c.toByte)
                       take(chars - 1)
                     } else  after(new String(buffer.toArray))
                   } else after(new String(buffer.toArray))
                 }
                 take(chars)
               }

              def consumeComment(chars: Int) =
                consume(chars)(_ match {
                  case '/' | '*' => false
                  case _ => true
                })(comments => {
                  val xhtml = toXHTML(knockoff(comments.trim.replace("@param", "<br/><strong>@param</strong>")))
                  val sw = new java.io.StringWriter
                  xml.XML.write(sw, xhtml, "utf-8", false, null)
                  out.write(sw.toString)
               })

              def consumeSrc(chars: Int) =
                consume(chars)({ c => true })({ src =>
                  println("src line '%s'" format(src))
                  out.write(src)})

              def code(f: => Unit) {
                out.write("""<td class="code"><pre class="brush: scala">""")
                f
                out.write("</pre></td>")
              }

              def doc(f: => Unit) {
                out.write("""<td class="docs">""")
                f
                out.write("</td>")
              }

              comments match {
                case Nil =>
                  code { consumeSrc(java.lang.Integer.MAX_VALUE) }
                out.write("</tr>")
                case comment :: tail =>
                  code { consumeSrc(comment.start - i) }
                out.write("</tr><tr>")
                doc { consumeComment(comment.length) }
					      next(comment.start + comment.length, tail)
              }
            }
           next(0, comments.toList)
          }
        }
        out.write(t.tail)
			}
		} // are source files
	} // end of gen out

	private def getSourceFile(unit: CompilationUnit): Option[File] = unit.source.file.file match {
		case null => None // code compiled from the repl has no source file
		case f: File => Some(f.getAbsoluteFile)
	}

  private def scan(unit: CompilationUnit) = {
    val comments = wrap.Wrappers.treeSet[Comments]

    val scanner = new syntaxAnalyzer.UnitScanner(unit) {
      override def init {}; def parentInit = super.init
    }

    implicit def iterator28(s: syntaxAnalyzer.UnitScanner) = {
			class CompatIterator extends Iterator[(Int, Int, Int)]	{
				def next = {
					type TD = { def offset: Int; def lastOffset: Int; def token: Int }
					class Compat { def prev: TD = null; def next: TD = null; def offset = 0; def token = 0; def lastOffset = 0 }
					implicit def keep27SourceCompatibility(a: AnyRef): Compat = new Compat// won't ever be called
					val offset = s.offset
					val token = s.token
					s.nextToken
					(offset, (s.lastOffset - offset) max 1, token)
				}
				def hasNext = s.token != Tokens.EOF
			}
			scanner.parentInit
			new { def iterator = new CompatIterator }
		}

    for((offset, length, code) <- scanner.iterator) {
     	code match {
			  case Tokens.COMMENT => comments += new Comments(offset, length)
			  case _ => ()
		  }
		}

    // Compability for 2.7: return no comments with the syntax of 2.8
		import unit._
		implicit def noCommentsInScala27(u: CompilationUnit) = new {
			def comments: Seq[Comment] = Seq.empty
		}
		implicit def rangePositionNeedsStartEndIn27(r: util.RangePosition) = new {
			def start = 0
			def end = 0
		}
	  for (Comment(_, pos) <- unit.comments) { comments += Comments(pos.start, pos.end-pos.start + 1) }
    comments
  }

	private def relativeSource(file: File) = new File(getRelativeSourcePath(file.getAbsoluteFile))

	private object Compat {
		def nameString(s: Symbol): String = s.fullNameString
		/** After 2.8.0.Beta1, fullNameString was renamed fullName.*/
		private implicit def symCompat(sym: Symbol): SymCompat = new SymCompat(sym)
		private final class SymCompat(s: Symbol) {
			def fullNameString = s.fullName; def fullName = sourceCompatibilityOnly
		}
		private def sourceCompatibilityOnly = error("For source compatibility only: should not get here.")
	}
}

/** Plugin definition module */
object DocPlugin {
  val PluginName = "scholia"
  val PluginDescription =
    "Producing literate style split documentation of src code"
  val BaseDirectoryOptionName = "base-directory:"
  val ProjectNameOptionName = "project-name:"
  val ProjectDescOptionName = "project-desc:"
}

/** Plugin definition */
class DocPlugin(val global: Global) extends Doc {
  import global._
  import DocPlugin._

  val name = PluginName
  val description = PluginDescription
  val components = List[PluginComponent](Component)

  private var baseDirectories: List[File] = Nil
  var projectName = "scholia"
  var projectDesc = "literate your scala documentation"

  lazy val classDirectory = new File(settings.outdir.value)

  override def processOptions(options: List[String], error: String => Unit) =
    for(op <- options) {
      if(op.startsWith(BaseDirectoryOptionName))
        baseDirectories = parseBaseDirectories(op.substring(BaseDirectoryOptionName.length))
      else if(op.startsWith(ProjectNameOptionName))
        projectName = op.substring(ProjectNameOptionName.length)
      else if(op.startsWith(ProjectDescOptionName))
        projectDesc = op.substring(ProjectDescOptionName.length)
      else  error("Unknown option %s " format op)
    }

  override val optionsHelp: Option[String] = {
    val prefix = "  -P:" + name + ":"
		val base = prefix + BaseDirectoryOptionName + "<paths> Set the base source directories."
    val projectName = prefix + ProjectNameOptionName + "<name> Set the name of the project being documented."
		Some("%s\n%s" format(base, projectName))
  }

  private def parseBaseDirectories(str: String): List[File] =
		str.split(File.pathSeparator).map(new File(_)).toList

  // For source compatibility between 2.7.x and 2.8.x
	private object runsBefore { def :: (s: String) = s }
	private abstract class CompatiblePluginComponent(afterPhase: String) extends PluginComponent {
		val runsAfter = afterPhase :: runsBefore
	}

  private object Component extends CompatiblePluginComponent("typer") {
    import global._
    import global.definitions._

    val global = DocPlugin.this.global
    val phaseName = DocPlugin.this.name
    def newPhase(prev: Phase) = new DocPhase(prev)
  }

  private class DocPhase(prev: Phase) extends Phase(prev) {
    def name = DocPlugin.this.name
    def run = generateOutput()
  }

  def getRelativeSourcePath(source: File): String =
    baseDirectories.flatMap { base => IO.relativize(base, source) } match {
      case Nil => source.getName
      case x :: Nil => x
      case xs => xs reduceLeft shortest
    }

  private [this] def shortest(a: String, b: String) = if(a.length<b.length) a else b
}

package forScope {
	class Sequence
	case class Comment(v: String, pos: util.RangePosition)
}
