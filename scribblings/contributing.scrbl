#lang scribble/manual

@(require "common.rkt"
          (for-label frosthaven-manager/defns))

@title{Contributor's Guide}

@section{Setting up for development}

@(define racket-lang.org "https://racket-lang.org/index.html")
@(define benknoble/frosthaven-manager "https://github.com/benknoble/frosthaven-manager")

@margin-note{On Windows without make, I recommend @tt|{choco install make}|.}

@itemlist[#:style 'ordered
          @item{Install @link[racket-lang.org]{Racket}. For example, on macOS with homebrew you might run @terminal|{brew install --cask racket}|}
          @item{Clone the @link[benknoble/frosthaven-manager]{benknoble/frosthaven-manager repository}.}
          @item{Install the package as a link. For example, you might go to the cloned directory and run @terminal|{make install}|}
]

@subsection{Running tests}

You can run automated tests with

@terminal|{
make test
}|

You may prefer to pass @tt|{RACO_TEST_ARGS=-x}| to skip modules without tests.

Manual GUI tests can be run by running the program or application,
@italic{e.g.}, with @tt|{racket gui/manager.rkt}| for the Frosthaven Manager.
Many GUI modules come with manual demos of the components they provide.

You can also build executables and distributions using the corresponding make
targets.

@subsection{Rebuild the package}

This makes sure that your local installation is compiled, that the docs are
up-to-date, and that the dependencies are appropriately adjusted.

@terminal|{
make fix-deps
}|

You should manually verify any dependency updates. The command @tt|{git diff
info.rkt}| will show any changes to the dependencies.

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
in @racketmodname[frosthaven-manager/qi/list2hash],
@racketmodname[frosthaven-manager/qi/utils], and
@racketmodname[frosthaven-manager/curlique].

Frosthaven Manager uses
@other-doc['(lib "racket/gui/easy/scribblings/gui-easy.scrbl")] to build
declarative GUIs. Local customizations to the observable operators can be found
in @racketmodname[frosthaven-manager/observable-operator].

Most of the game-related definitions are in
@racketmodname[frosthaven-manager/defns]. Support for elements and their images
is in @racketmodname[frosthaven-manager/elements]. Most GUI components are in
modules under @seclink["frosthaven-manager/gui"]{@tt{frosthaven-manager/gui}}.
The Frosthaven Manager application is
@racketmodname[frosthaven-manager/gui/manager] with state provided by
@racketmodname[frosthaven-manager/manager]. The monster database is manipulated
by @racketmodname[frosthaven-manager/monster-db].

The module @racketmodname[frosthaven-manager/enum-helpers] provides a utility
for modifying @tech[#:doc '(lib "rebellion/main.scrbl")]{enum types} from
@secref[#:doc '(lib "rebellion/main.scrbl")]{Enum_Types}.

@section{Debugging}

The main application has a @onscreen{Debug} menu that can run various tools,
such as getting a flickering canvas when GC occurs to diagnose memory issues.

There is also a @filepath{gui/deserialized-state.rkt} module that can be run
with

@terminal|{
racket -l frosthaven-manager/gui/deserialized-state <save-file>
}|

to debug the state saved with the @onscreen{Save Game} menu item; this is one
useful way to capture internal state and inspect it.

The main application accepts a @tt{--debug} flag to start GUI Easy's debugger
and Racket debugger, too. The latter can be connected to with @tt{raco dbg} if
you have the dbg-ui package.
