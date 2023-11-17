#lang scribble/manual

@(require (for-label racket/gui
                     racket/gui/easy
                     racket/gui/easy/contract))

@title{@tt{gui/static-table}}
@defmodule[frosthaven-manager/gui/static-table]

@defproc[(static-table
           [columns (listof label-string?)]
           [num-rows natural-number/c]
           [entry->columns (listof (-> any/c any/c))]
           [#:index->entry index->entry (-> natural-number/c natural-number/c) values]
           [#:entry->value entry->value (-> natural-number/c any/c) values]
           [#:selection |@selection| (maybe-obs/c
                                       (or/c #f
                                             exact-nonnegative-integer?
                                             (listof exact-nonnegative-integer?)))
            #f]
           [#:widths widths
            (maybe-obs/c
              (or/c #f
                    (listof
                      (or/c (list/c exact-nonnegative-integer?
                                    dimension-integer?)
                            (list/c exact-nonnegative-integer?
                                    dimension-integer?
                                    dimension-integer?
                                    dimension-integer?)))))
            #f])
         (is-a?/c view<%>)]{
A GUI view for static tables. The columns are labelled by @racket[columns], and
there are exactly @racket[num-rows] rows. Each row is indexed by a natural
number @racket[i] from @racket[0] to @racket[(sub1 num-rows)];
@racket[(entry->value (index->entry i))] computes a value @racket[v] on which
the functions in @racket[entry->columns] are called to compute the values of the
columns for that row. Each row is labelled with the entry @racket[(index->entry i)].

Summarizing: each row is indexed by a natural number in the range
[0,@racket[num-rows]). An entry is computed by @racket[index->entry]. A value is
computed from the entry by @racket[entry->value]. From this value, functions in
@racket[entry->columns] compute the elements of the row.

The selection is determined by @racket[|@selection|] as with @racket[table].

The column widths are calculated automatically based on @racket[columns], or are
provided as @racket[widths].
}
