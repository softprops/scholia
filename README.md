# scholia

literate your scala documentation

A reimagining of [sxr](https://github.com/harrah/browse), a Scala source browser, in the frame of literate style documentation like [docco](http://jashkenas.github.com/docco/)(node), [rocco](http://rtomayko.github.com/rocco/)(ruby), [schocco](http://rtomayko.github.com/shocco/)(shell), [dox](http://visionmedia.github.com/dox/)(node), [pycco](http://fitzgen.github.com/pycco/)(python), [marginalia](http://fogus.me/fun/marginalia/)(clojure).

## props & credit

This library is heavily influenced by the source of sxr. A **strong thank** you goes out to the hard work done by Mark Harrah on both sxr and sbt. Scala complier plugins are AWESOME.

## faq

* Why another documentation generator?

  * Scaladoc is heavy functionality but light on readability. Sxr is great for annotating source files in HTML format and navigating between sources as if you are were in an IDE but doesn't feel like documentation to me. Scholia Attempts to meet somewhere in between readable documentation and sxr documentation.

* Is this meant as a replacement for scaladoc?
  * No, scaladoc is the **standard** replacement for javadoc-style documentation in the scala community.

## usage

scholia is

    class Project(info: ProjectInfo) extends DefaultProject(info) with AutoCompilerPlugins {
      val scholia = compilerPlugin("me.lessis" %% "scholia" % "0.1.0-SNAPSHOT")
      override def compileOptions =
        CompileOption("-P:scholia:base-directory:" + mainScalaSourcePath.absolutePath) ::
          (compileOptions("-Xplugin-list") ++  super.compileOptions).toList
    }

Doug Tangren (softprops) 2011, Mark Harrah->[sxr](https://github.com/harrah/browse)
