#lang scribble/manual

@(require (for-label (except-in racket null)
                     frosthaven-manager/defns
                     racket/gui/easy
                     racket/gui/easy/contract
                     frosthaven-manager/manager
                     frosthaven-manager/monster-db
                     frosthaven-manager/parsers/formula))

@title{@tt{gui/monsters}}
@defmodule[frosthaven-manager/gui/monsters]

@defproc[(single-monster-picker
           [info-db info-db/c]
           [|@initial-level| (obs/c level/c)]
           [#:on-change on-change (-> single-monster-event/c any) void]
           [#:unavailable unavailable (set/c string?) empty])
         (is-a?/c view<%>)]{
A GUI view used to build a monster group by choosing the set, name, and included
monsters (along with their elite status and level). The available choices come
from @racket[info-db] less @racket[unavailable] (a set of monster names). The
callback @racket[on-change] is invoked each time changes are made. The default
monster level is specified by @racket[|@initial-level|].
}

@defproc[(simple-monster-group-view [|@mg| (obs/c monster-group?)])
         (is-a?/c view<%>)]{
A GUI view of a monster group showing a table of monsters and some other
information about the group.
}

@defproc[(multi-monster-picker
           [info-db info-db/c]
           [|@initial-level| (obs/c level/c)]
           [|@env| (obs/c env/c)]
           [#:on-change on-change (-> (or/c add-monster-event/c
                                            remove-monster-event/c)
                                      any) void])
         (is-a?/c view<%>)]{
A GUI view used to choose the monsters in a scenario: it composes
@racket[single-monster-picker] in order to allow selection and removal of entire
groups. The callback @racket[on-change] is invoked to notify of the addition or
removal of a group. Other parameters are used as in
@racket[single-monster-picker].
}

@defproc[(monster-group-view
           [|@mg| (obs/c monster-group?)]
           [|@ability-deck| (obs/c ability-decks?)]
           [|@monster-num| (obs/c (or/c #f monster-number/c))]
           [|@env| (obs/c env/c)]
           [#:on-select on-select (-> (or/c #f monster-number/c) any) void]
           [#:on-condition on-condition (-> monster-number/c condition? boolean? any) void]
           [#:on-hp on-hp (-> monster-number/c (-> number? number?) any) void]
           [#:on-kill on-kill (-> monster-number/c any) void]
           [#:on-new on-new (-> monster-number/c boolean? any) void]
           [#:on-swap on-swap (-> (or/c 'all monster-number/c) any) void]
           [#:on-move-ability-card on-move-ability-card (-> any) void]
           [#:on-max-hp on-max-hp (-> (-> (or/c 'normal 'elite) natural-number/c number?) any) void])
         (is-a?/c view<%>)]{
A GUI view used to display an entire monster group. See
@secref{Monster_Group_Controls}. An ability is displayed if an ability card is
present in @racket[|@ability-deck|]. The @racket[|@monster-num|] determines the
currently selected monster in the detailed portion of the view.

The callbacks function as follows:
@itemlist[
          @item{@racket[on-select] is given a new monster number when one is selected in the detailed view, or @racket[#false] if there are none.}
          @item{@racket[on-condition] is given a monster number, condition, and either @racket[#true] or @racket[#false] to indiciate whether the condition should be applied or removed.}
          @item{@racket[on-hp] is given a monster number and a procedure to update the monsters @racket[monster-current-hp].}
          @item{@racket[on-kill] is invoked with a monster number when that monster is killed.}
          @item{@racket[on-new] is invoked with a monster number and @racket[#true] if the monster is elite or @racket[#false] otherwise for a newly added monster.}
          @item{@racket[on-swap] is invoked with @racket['all] if all monsters should be swapped by @racket[swap-monster-group-elites], or with a monster number if only that monster should be swapped by @racket[swap-monster-elite].}
          @item{@racket[on-move-ability-card] is invoked when the top of the ability draw pile should be moved to the bottom using the ability deck previewer.}
          @item{@racket[on-max-hp] is invoked when adjusting all maximum HP of the group. The given procedure computes new maximum HP values as for @racket[monster-group-change-max-HP].}
]
}

@defproc[(db-view [|@info-db| (obs/c info-db/c)]
                  [|@ability-db| (obs/c ability-db/c)]
                  [|@monster-groups| (obs/c (listof monster-group?))])
         (is-a?/c view<%>)]{
A GUI view to display the hierarchical monster database, separated by
@racket[monster-info] and @racket[monster-ability].

Any pre-set monster groups will also be shown.
}

@defproc[(add-monster-group [|@info-db| (obs/c info-db/c)]
                            [|@initial-level| (obs/c level/c)]
                            [|@monster-names| (obs/c (set/c string? #:cmp 'dont-care #:kind 'dont-care))]
                            [|@env| (obs/c env/c)]
                            [#:on-group on-group (-> monster-group? any) void])
         any]{
Renders a dialog to add a monster group by invoking the callback
@racket[on-group] if one is selected. The value of @racket[|@initial-level|] is
used for the initial level of the group, which can be adjusted in the dialog.
Similarly, @racket[|@monster-names|] specifies which names are not available for
the new group.

Originally an internal part of the implementation of
@racket[multi-monster-picker] until it had uses in the main playing view.
}
