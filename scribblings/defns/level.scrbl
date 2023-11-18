#lang scribble/manual

@(require (for-label (except-in racket null)
                     frosthaven-manager/defns
                     frosthaven-manager/parsers/formula))

@title{Level Info}
@defmodule[frosthaven-manager/defns/level]

@defstruct*[level-info
             ([monster-level natural-number/c]
              [gold natural-number/c]
              [trap-damage natural-number/c]
              [hazardous-terrain natural-number/c]
              [exp natural-number/c])
             #:transparent]{
An instance of @racket[level-info] exposes characteristics of the level, such as
the monster level, value of gold, damage caused by traps and hazardous terrain,
and end-of-scenario experience.
}

@defthing[number-of-levels natural-number/c]{
A constant representing the number of possible levels, as opposed to what the
levels are.
}

@defthing[max-level natural-number/c]{
A constant representing the maximum level. The minimum level is 0.
}

@defthing[level/c contract?]{
A contract recognizing valid level numbers, used for both the scenario level and
monster levels.
}

@defthing[max-players natural-number/c]{
A constant representing the maximum number of players.
}

@defthing[num-players/c contract?]{
A contract recognizing a valid number of players.
}

@defproc[(get-level-info [level level/c]) level-info?]{
Returns the @racket[level-info] for the given @racket[level] number.
}

@defproc[(inspiration-reward [num-players num-players/c]) natural-number/c]{
Returns the amount of inspiration rewarded for completing a scenario based on
how many players participated in the scenario.
}
