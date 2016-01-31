latex-tools
===========

Files that I typically need to include in LaTeX documents.

* `references.bib` - a comprehensive BibTeX database
* `tcilatex.tex` - LaTeX file that's necessary when coauthors use
  Scientific Word
* `version_git.sh` and `version_git.make` - commands to generate
  git version information in a format that can be used inside
  LaTeX files.

License and copyright
=====================

Most of the files in the `latex-tools` repository are licensed under
the MIT "Expat" License.

> Permission is hereby granted, free of charge, to any person obtaining
> a copy of this software and associated documentation files (the
> "Software"), to deal in the Software without restriction, including
> without limitation the rights to use, copy, modify, merge, publish,
> distribute, sublicense, and/or sell copies of the Software, and to
> permit persons to whom the Software is furnished to do so, subject to
> the following conditions:
>
> The above copyright notice and this permission notice shall be
> included in all copies or substantial portions of the Software.
>
> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
> EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
> MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
> IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
> CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
> TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
> SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

* The file `version_git.sh` comes from the [Julia project][julia] and
  has the following copyright:

  > Copyright (c) 2009-2015: Jeff Bezanson, Stefan Karpinski, Viral
  > B. Shah, and other contributors:
  > https://github.com/JuliaLang/julia/contributors

  This file is MIT licensed.

[julia]: http://julialang.org

* `tcilatex.tex` comes from Scientific Workplace

* The other files are copyright Gray Calhoun and are under the MIT
  license.