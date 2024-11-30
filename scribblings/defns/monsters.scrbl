#lang scribble/manual

@(require (for-label (except-in racket
                                null
                                newline)
                     pict
                     frosthaven-manager/contracts
                     frosthaven-manager/defns
                     frosthaven-manager/parsers/formula
                     frosthaven-manager/gui/rich-text-display
                     (submod frosthaven-manager/gui/rich-text-display model)))

@title{Monster Cards}
@defmodule[frosthaven-manager/defns/monsters]

@defstruct*[monster-stats
             ([max-hp (or/c positive-integer? string?)]
              [move (or/c #f natural-number/c)]
              [attack (or/c natural-number/c string?)]
              [bonuses (listof string?)]
              [effects (listof string?)]
              [immunities (listof string?)])
             #:prefab]{
The monster statistic representation, usually used with pre-fabs.
}

@defstruct*[monster-info
             ([set-name string?]
              [name string?]
              [normal-stats (apply list/c (make-list number-of-levels monster-stats?))]
              [elite-stats (apply list/c (make-list number-of-levels monster-stats?))])
             #:prefab]{
The monster information representation, often for reading pre-fab structs.
}

@defstruct*[monster-ability
             ([set-name string?]
              [name string?]
              [initiative initiative?]
              [abilities (listof (listof (or/c string? pict?)))]
              [shuffle? boolean?])
             #:prefab]{
The monster ability representation, often for reading pre-fab structs.

Note that pre-fab syntax does not permit @racket[path?] or @racket[pict?] objects.

The monster abilities are a list of sub-abilities, much like a character's
ability card. Each sub-ability is a list of parts, which may include pictures
such as for embedded AoE diagrams.
}

@defthing[monster-number/c contract?]{
A contract that recognizes valid monster numbers.
}

@defstruct*[monster
             ([number monster-number/c]
              [elite? boolean?]
              [current-hp natural-number/c]
              [conditions (listof condition?)])
             #:transparent]{
A @racket[monster] captures the individual status of a monster, but not its
game statistics. Those are listed in its parent @racket[monster-group].

Prefer the smart constructor @racket[make-monster].

Serializable.
}

@defstruct*[monster-group
             ([set-name string?]
              [name string?]
              [level level/c]
              [normal-stats monster-stats?]
              [elite-stats monster-stats?]
              [monsters (listof monster?)])
             #:transparent]{
A @racket[monster-group] describes a group of @racket[monster]s and their stats.

Prefer the smart constructor @racket[make-monster-group] and the update
functions, which maintain an invariant of monsters sorted by eliteness and
number.

Serializable.
}

@defproc[(monster-stats-max-hp* [stats monster-stats?] [env env/c])
         positive-integer?]{
Calculates the maximum HP value of @racket[stats], which may be a formula.
}

@defproc[(monster-stats-attack* [stats monster-stats?] [env env/c])
         positive-integer?]{
Calculates the attack value of @racket[stats], which may be a formula.
}

@deftogether[(
  @defproc[(monster-stats-bonuses-string [m monster-stats?]) string?]
  @defproc[(monster-stats-effects-string [m monster-stats?]) string?]
  @defproc[(monster-stats-immunities-string [m monster-stats?]) string?]
)]{
Returns a single string containing all the bonuses, effects, or immunities.
}

@defproc[(monster-ability-name->text [ability (or/c #f monster-ability?)])
         string?]{
Returns a string suitable for display to indicate the name of a possibly-absent
monster ability.
}

@defproc[(monster-ability-initiative->text [ability (or/c #f monster-ability?)])
         string?]{
Returns a string suitable for display to indicate the initiative of a
possibly-absent monster ability.
}

@defproc[(monster-ability-ability->rich-text
           [ability-text (listof (or/c string? pict?))]
           [mg monster-group?]
           [env env/c])
         (listof (or/c string? pict? pict/alt-text? newline?))]{
Format a single monster ability from an ability card as a rich text sequence,
compatible with @racket[rich-text-display].
}

@itemlist[
          @item{an extra @racket[`(aoe-pict ,aoe-pict)] means the ability had an area of effect specified by the pict.}
]
}

@defproc[(make-monster [info monster-info?]
                       [level level/c]
                       [number monster-number/c]
                       [elite? boolean?]
                       [env env/c])
         monster?]{
Populates the resulting @racket[monster] based on the statistics from
@racket[info] and @racket[elite?]. Formulas are calculated using @racket[env].
}

@defproc[(make-monster-group [info monster-info?]
                             [level level/c]
                             [num+elite?s (and/c (listof (cons/c monster-number/c boolean?))
                                                 (unique-with/c car any/c))]
                             [env env/c])
         monster-group?]{
Creates a @racket[monster-group] at level @racket[level] based on the statistics
from @racket[info].

The @racket[num+elite?s] parameter provides a mapping from (unique) monster
numbers to their elite status. Only monster numbers in the mapping are added to
the @racket[monster-group].

Formulas are calculated using @racket[env].
}

@defproc[(get-monster-stats [mg monster-group?] [m monster?]) monster-stats?]{
Retrieves the corresponding @racket[monster-group-normal-stats] or
@racket[monster-group-elite-stats] based on @racket[(monster-elite? m)]. The
monster @racket[m] is assumed to be part of the group @racket[mg].
}

@defproc[(monster-at-max-health? [m monster?] [s monster-stats?] [env env/c]) boolean?]{
True if-and-only-if @racket[(monster-current-hp m)] is
@racket[(monster-stats-max-hp* s env)]. The stats @racket[s] are assumed to
correlate to the monster @racket[m].
}

@defproc[(monster-dead? [m monster?]) boolean?]{
True if-and-only-if @racket[(monster-current-hp m)] is @racket[0].
}

@defproc[((monster-update-condition [c condition?] [on? boolean?])
          [m monster?])
         monster?]{
Transforms @racket[(monster-conditions m)] by adding the condition
@racket[c] if @racket[on?] or removing it otherwise.
}

@defproc[(monster-expire-conditions [m monster?]) monster?]{
Remove @racket[expirable-conditions] from @racket[m].
}

@defproc[((monster-update-hp [f (-> number? number?)])
          [m monster?])
         monster?]{
Transforms @racket[(monster-current-hp m)] by @racket[f]. If the result is not
@racket[positive?] the update is ignored.
}

@defproc[((monster-group-update-num
            [n monster-number/c]
            [f (-> monster? monster?)])
          [mg monster-group?])
         monster-group?]{
Transforms @racket[(monster-group-monsters mg)] by transforming the monster
numbered @racket[n] by @racket[f].
}

@defproc[((monster-group-remove [n monster-number/c])
          [mg monster-group?])
         monster-group?]{
Removes the monster numbered @racket[n] from @racket[(monster-group-monsters mg)].
}

@defproc[((monster-group-add [n monster-number/c] [elite? boolean?] [env env/c])
          [mg monster-group?])
          monster-group?]{
Adds the monster numbered @racket[n] to @racket[(monster-group-monsters mg)]. It
is elite if-and-only-if @racket[elite?] is @racket[#true].
}

@defproc[(monster-group-first-monster [mg monster-group?])
         (or/c #f monster-number/c)]{
The number of the first monster in the monster group @racket[mg], or
@racket[#false] if there are no such monsters.
}

@defproc[(monster-group-update-level [mg monster-group?]
                                     [info monster-info?]
                                     [new-level level/c])
         monster-group?]{
Converts @racket[mg] to @racket[new-level] by extracting stats from
@racket[info].
}

@defproc[(monster->hp-text [m monster?] [ms monster-stats] [env env/c])
         string?]{
Formats the string @racket["HP: current/max"] for the monster @racket[m].
}

@defproc[(swap-monster-group-elites [mg monster-group?])
         monster-group?]{
The same monster group, but with all elite monsters normal and vice-versa.
}

@defproc[(swap-monster-elite [m monster?])
         monster?]{
The same monster, but normal instead of elite and vice-versa.
}

@defproc[(monster-group-change-max-HP [mg monster-group?] [f (-> (or/c 'normal 'elite) natural-number/c number?)] [env env/c])
         monster-group?]{
Updates a @racket[monster-group]'s maximum HP value in all statistics by
applying a procedure.
}
