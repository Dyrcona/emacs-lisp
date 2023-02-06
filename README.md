# GNU Emacs Initialization

Dyrcona's initialization scripts and libraries for GNU Emacs.

## Description

This repository holds Jason Stephenson's current initialization
scripts and other code for GNU Emacs, developed over 30 years of using
GNU Emacs for programming and other text editing chores.  They not
only set up the work environment on 3 different systems but also
provide commands and skeletons to automate the drudgery of working
with certain kinds of programs.  They were recently spiffed up and
pared down for sharing with the public.

Perhaps you'll find some inspiration for improving your GNU Emacs
experience and automating some of your routine chores?

## Getting Started

### Dependencies

You will need a recent version of GNU Emacs.  The author currently
uses these scripts with versions 26.3, 27.1, and 28.2.

You might need a few other packages and programs such as slime, Steel
Bank Common Lisp, Guile, Python 3, etc.

### Installing

If you want to use these files as they are, you should copy them to
your `~/.emacs.d/` directory.

It is NOT recommended that you use them as-is because much of the code
found herein is specialized for the kinds of work that the author
does.  However, they may serve as examples to others who need to
automate similar kinds of tasks.

## Authors

[Jason Stephenson](https://www.sigio.com/) is responsible for most of
the code here.

Some portions were taken from, or inspired by, other sources as listed
in the [Acknowledgments](#acknowledgments) and comments in the code.

## License

The scripts are licensed under the terms of the GNU General Public
License - see [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

Two files, [buffer-advice.el](elisp/buffer-advice.el) and
[unscroll.el](elisp/unscroll.el), were largely taken from
[Writing GNU Emacs Extensions](https://www.oreilly.com/library/view/writing-gnu-emacs/9781449395056/)
by Bob Glickstein, published in 1997 by O'Reilly Media & Associates,
Inc.  No license other than copyright is mentioned in the text, though
Appendix C suggests sharing code under the GNU General Public License.

The `funcall-region` function in
[funcall-region.el](elisp/funcall-region.el) was taken from a
[StackOverflow answer](https://stackoverflow.com/a/6541072) by
[db48x](https://stackoverflow.com/users/823846/db48x).

Code for moving within skeletons was taken from [Emacs
Wiki](https://www.emacswiki.org/emacs/SkeletonMode#h5o-15) as
indicated in the comments of [my-skeletons.el](elisp/my-skeletons.el).
