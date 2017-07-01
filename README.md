# mdpeg

[![CircleCI](https://circleci.com/gh/DRouh/mdpeg.svg?style=svg)](https://circleci.com/gh/DRouh/mdpeg)

*mdpeg* is a markdown parser that uses [parsing expression grammar](http://bford.info/packrat/) also known as PEG. 

PEG definitions have been heavily influenced by [markdown-peg](https://github.com/jgm/markdown-peg) project.

## Getting Started

### Parsing a Markdown file into an AST

Following is an example of how to parse text file input into an AST.

``` scala
import org.mdpeg.MarkdownParser

val fileText = scala.io.Source.fromFile("input.md").mkString

MarkdownParser.parse(fileText) match {
  case Left(errors) => println(errors)
  case Right(ast) => println(ast)
}
```

### Transforming an AST into HTML string

Following is an example of how to transform an AST into HTML string.

``` scala
import org.mdpeg.MarkdownParser

MarkdownParser.transformToHtml(ast) match {
  case Left(error) => println(error) 
  case Right(html) => println(html)
}
```

## Examples
More examples you can find in the `examples` folder in the repository.


## Current status of development
Actual information about the status and further work you can find on the project's [issues](https://github.com/DRouh/mdpeg/issues) section.
