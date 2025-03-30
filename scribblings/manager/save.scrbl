#lang scribble/manual

@(require (for-label racket
                     frosthaven-manager/manager))

@title{@tt{manager/save}}
@defmodule[frosthaven-manager/manager/save]

@defproc[(do-save-game [s state?]) any]{
Prompts the user to save the game to a file of their choice.
}

@defproc[(do-load-game [s state?]) any]{
Prompts the user to load the game from a file of their choice.
}

@defproc[((save-game [s state?]) [p path-string?]) any]{
Save a game to @racket[p].
}

@defproc[((load-game [s state?]) [p path-string?]) any]{
Load a game from @racket[p].
}
