#lang scribble/manual

@(require (for-label racket/gui
                     racket/gui/easy
                     racket/gui/easy/contract
                     frosthaven-manager/gui/stacked-tables
                     (except-in frosthaven-manager/observable-operator #%app)))

@title{@tt{gui/stacked-tables}}
@defmodule[frosthaven-manager/gui/stacked-tables]

@defproc[(stacked-tables
           [#:topleft? topleft? boolean? #t]
           [#:panel panel (-> (is-a?/c view<%>) ... (is-a?/c view<%>)) hpanel]
           [|@data| (obs/c (vectorof any/c))]
           [final-view (-> (obs/c (or/c #f any/c)) (is-a?/c view<%>))]
           [column1 column?]
           [column-spec column?] ...)
         (is-a?/c view<%>)]{
A view of @racket[|@data|] using stacked tables. The tables are horizontal,
left-to-right by default. Supplying @racket[vpanel] for @racket[panel] makes the
stack vertical. When @racket[topleft?] is true, the first table is on the left
or top of the stack. Otherwise it is on the right or bottom, reversing the order
of subsequent tables.

The stack of tables is determined by @racket[column1] and each
@racket[column-spec]. The first is always @racket[column1].

Starting with @racket[|@data|] and @racket[column1], a table is added to the
stack. The table's title is given by @racket[column-title]. The labels for the
items in the table come from applying @racket[column-entry->label] to the values
in the data. When a value is selected, the data for the next table and
@racket[column-spec] is produced by @racket[column-entry->next] on the
selection. This value is automatically wrapped in @racket[vector] as needed.

This process continues, adding tables to the stack whose data depends on
previous data and selections, until the final table and @racket[column-spec] are
added. The final selection, which is @emph{not} automatically vectorized, is
given to @racket[final-view]. The resulting view is also added to the stack.

The intermediate data produced by @racket[column-entry->next] is automatically
emptied when no value is selected previously. In contrast, @racket[final-view]
needs to handle the case that no data has yet been selected. A common pattern
is to compute a default value:
@codeblock|{
(stacked-tables
  @data
  (λ (@x?) ... (@~> @x? (or _ default)) ...)
  ...)
}|
}

@defstruct*[column ([title string?]
                    [entry->label (-> any/c string?)]
                    [entry->next (-> any/c (or/c any/c (vectorof any/c)))])]{
A column specification for @racket[stacked-tables], which explains how the
specification is used.

A note about @racket[column-entry->next]: you almost certainly want to return a
@racket[vector] for all but (possibly) the last @racket[column.] Intermediate
@racket[column]s likely have multiple choices. As a convenience, when there is
only one, you may omit the vector. For the final @racket[column], you likely
want to omit the vector unless the selected data is one: the data here is the
final selection, of which there should probably be one.
}
