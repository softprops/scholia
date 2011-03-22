/** hopefully a better drop-in replacement for the offical sh scala brush
  * http://alexgorbatchev.com.s3.amazonaws.com/pub/sh/3.0.83/scripts/shBrushScala.js
  * @see http://www.scala-lang.org/docu/files/ScalaReference.pdf
  */
function BackTrackRegExp(pattern) {
	pattern = pattern + '(?=[[\\]{}(),\\s])';
	this.regex = new RegExp(pattern, 'g');
	this.lookBehind = /[\[\]{}(),\s]$/;
}

BackTrackRegExp.prototype.exec = function (str) {
	var match, leftContext;
	while (match=this.regex.exec(str)) {
		leftContext = str.substring(0, match.index);
		if (this.lookBehind.test(leftContext)) {
			return match;
		}
		else {
			this.regex.lastIndex = match.index + 1;
		}
	}
	return null;
};

SyntaxHighlighter.brushes.Scala = function() {

  var reservedWords = [
        'abstract case catch class def',
        'do else extends final',
        'finally for forSome if implicit',
        'import lazy match new null',
        'object override private protected',
        'return sealed super this throw',
        'trait try type val',
        'var while with yield'].join(' ')
  var preprocessors = ['^import ^package'].join(' ')
        //' # :< %> :> -> <= = : @ _ '].join(' ');
  var ops = ['; += + - / * = ~ ^ | || == -> >>> >> # <<< << -> <- # :< : _ <% @'].join('');

  this.getKeywords = function(keywordStr) {
    // quote special characters
		keywordStr = keywordStr.replace(/[\-\[\]{}()*+?.\\\^$|,#\;]/g, "\\$&");
		// trim whitespace and convert to alternatives
		keywordStr = keywordStr.replace(/^\s+|\s+$/g, '').replace(/\s+/g, '|');
		// create pattern
		return '(?:' + keywordStr + ')';
  };


  this.regexList = [
      /* comments */
      { regex: SyntaxHighlighter.regexLib.singleLineCComments,
        css: 'comments'},
      { regex: SyntaxHighlighter.regexLib.multiLineCComments,
        css: 'comments' },
      /* strings */
      { regex: SyntaxHighlighter.regexLib.multiLineDoubleQuotedString,
        css: 'string' },
      /* chars */
      { regex: SyntaxHighlighter.regexLib.singleQuotedString,
        css: 'string' },
      /* constants/types */
      { regex: /\b([A-Z]([A-Za-z0-9_]*)?)\b/g,
        css: 'constants' },
      /* symbols */
      { regex: /\'\w+/g,
        css: 'preprocessor' },
      /* `magic` symbols */
      { regex:/\`\w+\`/g,
        css: 'preprocessor' },
      /* octal/hex numerics */
      { regex: /[+-]?\b0[x][0-9A-F]+([e][+-]?\d+)?([dfl])?\b/gi,
        css: 'value' },
      /* int/dec numberics */
      { regex: /[+-]?\b\d+(\.\d*)?([e][+-]?\d+)?([dfl])?\b/gi,
        css: 'value' },
      /* booleans */
      { regex: /\b(true|false)\b/g,
        css: 'value' },
      /* tuples */
      { regex: /\(|\)/g,
			  css: 'keyword' },
      /* type brackets */
      { regex: /\[|\]/g,
        css: 'keyword' },
      { regex: /@[A-Za-z\.]+/g,
        css: 'preprocessor'
      },
      /* type annotations */
     /* { regex: /\[(\b)?/g[+-],
        css: 'functions' },*/
      /* amperstands */
      { regex: /&amp;/g,
        css: 'keyword' },
      /* reserved words */
      { regex: new BackTrackRegExp(this.getKeywords(reservedWords)),
        css: 'preprocessor' },
      /* operators */
      { regex: new BackTrackRegExp(this.getKeywords(ops)),
        css: 'functions' },
      /* operators */
      { regex: new BackTrackRegExp(this.getKeywords(preprocessors)),
        css: 'functions' }
  ];

  this.forHtmlScript(SyntaxHighlighter.regexLib.scriptScriptTags);
};

SyntaxHighlighter.brushes.Scala.prototype = new SyntaxHighlighter.Highlighter();
SyntaxHighlighter.brushes.Scala.aliases = ['scala'];