#lang scribble/manual

@(require "common.rkt"
          (for-label "../defns.rkt"))

@title{Contributor's Guide}

@section{Setting up for development}

@(define racket-lang.org "https://racket-lang.org/index.html")
@(define benknoble/frosthaven-manager "https://github.com/benknoble/frosthaven-manager")

@margin-note{Note that the name of the directory will determine the package
name. In the guide below, some commands refer to modules and some refer to
package name. I have used a package name of @tt{frosthaven-manager} everywhere,
so I recommend cloning the code into a directory with that name.}

@itemlist[#:style 'ordered
          @item{Install @link[racket-lang.org]{Racket}. For example, on macOS with homebrew you might run @tt|{brew install --cask racket}|.}
          @item{Clone the @link[benknoble/frosthaven-manager]{benknoble/frosthaven-manager repository}.}
          @item{Install the package as a link by running @tt|{raco pkg install}| from the cloned directory.}
]

@subsection{Running tests}

You can run automated tests with

@terminal|{
raco test -xp frosthaven-manager
}|

Manual GUI tests can be run by running the program or application, @italic{e.g.},
with @tt|{racket manager.rkt}| for the Frosthaven Manager. Many GUI modules come
with manual demos of the components they provide.

@subsection{Rebuild the package}

This makes sure that your local installation is compiled, that the docs are
up-to-date, and that the dependencies are appropriately adjusted.

@terminal|{
raco setup --check-pkg-deps --unused-pkg-deps --fix-pkg-deps frosthaven-manager
}|

You should manually verify any dependency updates.

@subsection{View the local documentation}

Run the following command:

@terminal|{
raco docs frosthaven-manager
}|

Or, to look for documentation for a specific thing, like @racket[creature], run

@terminal|{
raco docs creature
}|

You'll want to make sure you select the search entry related to the Frosthaven
Manager.

@section{Contributing guidelines}

All contributors are expected to behave like polite, respectful adults. Anything
less will not be tolerated. This project is committed to maintaining respect and
equality for all people.

Many forms of contributing are accepted, including helping with social
processes, maintaining text such as documentation, and writing code. If you
don't want to contribute code, you can still contribute to Frosthaven Manager.

Contributions to code should come with @emph{best effort} documentation. If you
aren't used to Scribble yet, write some Markdown or plain text and let us help
you convert it to Scribble. If you aren't sure what to write, make an attempt
and let us help you improve it.

Contributions to code may also come with tests. This is preferred but not
required. It should be possible to somehow demonstrate that the contribution
works correctly.

Style-wise: prefer the local customizations documented in
@secref{Navigating_the_code}. Prefer functional, immutable idioms where
possible.

@section{Navigating the code}

A full reference is provided at @secref{Developer_Reference}.

Frosthaven Manager makes extensive use of
@other-doc['(lib "qi/scribblings/qi.scrbl")]. Local customizations can be found
in @racketmodlink[frosthaven-manager/qi].

Frosthaven Manager uses
@other-doc['(lib "racket/gui/easy/scribblings/gui-easy.scrbl")] to build
declarative GUIs. Local customizations to the observable operators can be found
in @racketmodlink[frosthaven-manager/observable-operator].

Most of the game-related definitions are in
@racketmodname[frosthaven-manager/defns]. Support for elements and their images
is in @racketmodname[frosthaven-manager/elements]. Most GUI components are in
modules under @seclink["frosthaven-manager/gui"]{@tt{frosthaven-manager/gui}}.
The Frosthaven Manger application is @racketmodname[frosthaven-manager/manager].
The monster database is manipulated by
@racketmodname[frosthaven-manager/monster-db].

The module @racketmodname[frosthaven-manager/enum-helpers] provides a utility
for modifying @tech[#:doc '(lib "rebellion/main.scrbl")]{enum types} from
@secref[#:doc '(lib "rebellion/main.scrbl")]{Enum_Types}.
