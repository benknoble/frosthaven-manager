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
with @tt|{racket manager.rkt}| for the Frosthaven Manager.

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
