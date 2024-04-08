#lang scribble/manual

@(require "common.rkt"
          "../monster-db.rkt"
          racket/file
          racket/port
          pict
          (for-label (except-in racket null)
                     frosthaven-manager/defns)
          frosthaven-manager/aoe-images)

@title{Programming a Scenario}

There are many elements of a scenario, such as which characters are playing at
what level, what the loot is, and which foes are involved. This document
describes how you can program these elements so that the Frosthaven Manager can
load a whole scenario without asking you to input this information every time
you play.

Monster information, including statistics and abilities is stored in a
"bestiary." See @secref[select-monster-db-tag] for how the bestiary is loaded
into the game. We'll cover how to write bestiaries in
@secref{Bestiary_Format_by_Example}.

Instead of a bestiary, you can use a foe specification, which describes what
monsters you'll face in a scenario in addition to everything a bestiary
describes. This can also be loaded into the game; see
@secref[select-monster-db-tag]. You can use all the features of bestiaries to
define or import monsters and abilities, and there are extra features to specify
exactly what foes you'll face in the scenario. We'll cover writing foe
specifications starting in @secref{Foe_Specification_by_Example}.

A good pattern for personal organization is to use bestiaries to
define a collection of monsters, and foe specifications to define
scenario-specific foes by importing the bestiary.

You can also define a stickered set of loot cards for your scenario to avoid
re-stickering cards each scenario. We'll cover how to do so in
@secref{Programming_Loot}.

@margin-note{Interested in contributing to Frosthaven Manager? Help me build
editors to make creating custom bestiaries and scenarios easier!}

@section{Where Can I Store Scenario Programs?}

Simply: anywhere! Any plain-text file will do.

@margin-note{Note that some "text editors" are for rich text. Do not use
Microsoft Word to edit plain-text files. On macOS, be careful with the
TextEdit.app---it has options to edit rich-text format (RTF) files, which should
be avoided when working with plain text.}

Since the contents of the file will be a program in a Racket dialect, the
traditional suffix is @filepath{rkt}. So you might create a plain-text file
named @filepath{my_bestiary.rkt} to contain your bestiary or
@filepath{scenario_123_foes.rkt} for the foes specification for a scenario.
We'll cover the exact formats of various scenario programs in the next sections,
starting with @secref{Bestiary_Format_by_Example}.

@section{Bestiary Format by Example}

Let's start with an example bestiary. This is the one that comes with
Frosthaven Manager to try things out:

@(typeset-code (file->string default-monster-db)
               #:context #'here
               #:line-numbers 1)

This is a bit long, but we can already identify several distinct components:
@itemlist[#:style 'ordered
          @item{Line 1 marks the file as containing a bestiary.}
          @item{Lines 3, 38, and 60 begin monsters.}
          @item{Monsters have names and many stats, like on lines 4--22.}
          @item{Lines 25 and 82 begin ability decks.}
          @item{Ability decks have ability cards, lik on lines 28--35.}
]

There is a lot of whitespace to help provide a visual feel of the layout. You
are encouraged to use whitespace for layout, but the Frosthaven Manager will
ignore it in most cases.

You might notice double-quotes @tt{"} around names like @racket["guard"] or
@racket["hynox archer"]. These denote @tech{game text}, which is always enclosed
by double-quotes to group the text together.

You might notice some numbers, like on line 5, are not in quotes. These are
@tech{game numbers}, like max HP. For Frosthaven Manager you should only see
positive natural numbers, like @racket[1], @racket[2], @racket[3], @racket[42],
etc. Contrast these with numbers written in quotes, like in @racket["shield 3"]
on line 21: these numbers are part of the @tech{game text}, like in an ability.

With these observations, we're ready to write a monster definition!

@subsection{Write Your Own Monster}

@; Use codeblock in this section so that the examples look the same as the one
@; rendered by typeset-code above.

We'll start with the monster information, which contains all the statistics
needed for different levels of the monster. To begin, we write
@margin-note{Don't forget to put @(hash-lang)
@racketmodname[frosthaven-manager/bestiary] at the top of the file!}

@codeblock|{begin-monster}|

This indicates a monster definition, as observed. Every part
of the information goes between this opener and the closer @code|{end-monster}|.

The next two things are the Monster name and Set name. For example, a Hobgoblin
Warrior would have a Monster name of "Hobgoblin Warrior" and a Set name of
"Warrior." Both of these are @tech{game text}, so don't forget the quotes. The
Set name belongs in parentheses because in many cases it can be omitted, in
which case the last word of the Monster name is used. So we can write either

@codeblock|{"Hobgoblin Warrior"}|

or

@codeblock|{"Hobgoblin Warrior" ("Warrior")}|

Next comes the normal and elite monster stats. Each stat block is sandwhiched in
square brackets like @code|{[ …stat block… ]}|. Inside, a stat block always
starts with a level, which is a @tech{game number} between 0 and 7, and a type,
which is either @code|{normal}| or @code|{elite}|. Then we need to add the
actual stats. Once you've seen one example of monster stats, you've seen most of
them!

The three mandatory elements of the monster stats are max HP, base movement,
and base attack. These are all @tech{game numbers}. Let's say the Hobgoblin
Warrior has 4 max HP, move 2, and attack 2 at level 0:

@codeblock|{[0 normal [HP 2] [Move 2] [Attack 2]]}|

We can rearrange the @code|{HP}| and other stats, as long as they come after the
level and the type and are within the square brackets.

The next three elements of the monster stats are optional. They are persistent
bonuses, attack effects, and condition immunities. All three are @tech{game
text}, which means quotes. Bonuses are things like "Shield 1." Attack effects
are things like "2 Targets" and "Pierce 1." Condition immunities are things like
"Poison." For many monsters, all three groups will be empty, so you can leave
them out. Let's use the examples in this paragraph for the Hobgoblin Warrior,
even though it might be far too strong for level 0, just to show how to write
them:

@codeblock|{
[0 normal [HP 2] [Move 2] [Attack 2]
          [Bonuses {"Shield 1"}]
          [Effects {"2 Targets" "Pierce 1"}]
          [Immunities {"Poison"}]]
}|

Notice that we can put multiple, whitespace-separated chunks of @tech{game text}
in curly brackets, like @code|{ {"one" "two"} }|. Since there can be many
bonuses, effects, or immunities, we collect them in a list like this.

Now you've seen it all: we need to write 16 total monster stats for the
Hobgoblin Warrior, which we'll do by making up the numbers as we go. Then we
finish out with @code|{end-monster}|. Normally we would probably refer to an
existing Frosthaven monster stats card, but you can also make your own.

@codeblock|{
begin-monster
    "Hobgoblin Warrior"

    [0 normal [hp 4] [move 2] [attack 2]]
    [1 normal [hp 5] [move 2] [attack 2]]
    [2 normal [hp 6] [move 3] [attack 3]]
    [3 normal [hp 7] [move 3] [attack 3]]
    [4 normal [hp 8] [move 3] [attack 4]]
    [5 normal [hp 9] [move 3] [attack 4]]
    [6 normal [hp 10] [move 4] [attack 5]]
    [7 normal [hp 11] [move 4] [attack 5]]

    [0 elite [hp 6] [move 3] [attack 3] [bonuses {"Shield 1"}] [effects {"2 Targets" "Pierce 1"}] [immunities {"Poison"}]]
    [1 elite [hp 7] [move 3] [attack 3] [bonuses {"Shield 1"}] [effects {"2 Targets" "Pierce 1"}] [immunities {"Poison"}]]
    [2 elite [hp 8] [move 4] [attack 4] [bonuses {"Shield 2"}] [effects {"2 Targets" "Pierce 1"}] [immunities {"Poison"}]]
    [3 elite [hp 9] [move 4] [attack 4] [bonuses {"Shield 2"}] [effects {"2 Targets" "Pierce 1"}] [immunities {"Poison"}]]
    [4 elite [hp 10] [move 4] [attack 5] [bonuses {"Shield 2"}] [effects {"2 Targets" "Pierce 1"}] [immunities {"Poison"}]]
    [5 elite [hp 11] [move 4] [attack 5] [bonuses {"Shield 2"}] [effects {"2 Targets" "Pierce 1"}] [immunities {"Poison"}]]
    [6 elite [hp 12] [move 5] [attack 6] [bonuses {"Shield 3"}] [effects {"2 Targets" "Pierce 1"}] [immunities {"Poison"}]]
    [7 elite [hp 13] [move 5] [attack 6] [bonuses {"Shield 3"}] [effects {"2 Targets" "Pierce 1"}] [immunities {"Poison"}]]
end-monster
}|

That's an entire monster definition for Hobgoblin Warriors. But right now it
doesn't have any ability cards!

Let's write an ability deck. Like before, we start at the beginning:

@codeblock|{begin-ability-deck}|

Next we have the Set name to which the ability card belongs. Recall that, for
example, archers all use the same deck. Here we want to use the "Warrior" set.

@codeblock|{"Warrior"}|

Now we need some cards. Like stats, these go in square brackets in a particular
format. They start with name and initiative.

Let's write the "Crushing Blow" card. I think this is a slow ability, so let's
go with 65:

@codeblock|{
["Crushing Blow" 65]
}|

All ability cards have a list of abilities, which are @tech{game text}.
I think Crushing Blow should have a short move and a big attack, so lets add two
abilities for that:

@codeblock|{
["Crushing Blow" 65 {"Move 2" "Attack +4, Muddle"}]
}|


The last thing we need is to indicate whether this is a "shuffle" card or
not---recall that some ability cards trigger a re-shuffle of the ability deck
for that set. As usual, if the deck ever runs out, it is also re-shuffled.

Crushing Blow doesn't seem like the kind of ability that needs to shuffle the
ability deck, so we don't need to add anything else to that card. But an ability
like Rallying Cry could trigger a shuffle:

@codeblock|{
["Rallying Cry" 10 shuffle {"Shield 1" "Heal 2, range 2"}]
}|

Since we wanted this card to trigger a shuffle, we wrote @code|{shuffle}|
between the initiative and the list of abilities.

To finish off the deck, we need 6 more cards! We'll gloss over those here.
Here's what it would look like:

@codeblock|{
begin-ability-deck
    "Warrior"

    ["Crushing Blow" 65 {"Move 2" "Attack +4, Muddle"}]
    ["Rallying Cry" 10 shuffle {"Shield 1" "Heal 2, range 2"}]
    …
end-ability-deck
}|

And that's how you write a monster or ability deck!

@subsection{Using Multiple Bestiaries}

It's possible to import the monsters from one bestiary into another. The command
@racket[import-monsters] does this when given a filename written as @tech{game
text}. For example:

@codeblock|{
import-monsters "guards.rkt"
}|

would import all the monsters and ability decks in the file
@filepath{guards.rkt} for use in the current bestiary.

Use the @filepath{/} slash character to separate folders and directories from
filenames. If there is no slash, the bestiary from which to import is assumed to
be in the same folder or directory as the bestiary containing the import
command. Use @filepath{..} to mean one level above the folder or directory
containing the bestiary with the import command, so that the following command
imports @filepath{monsters.rkt} from one directory above the current bestiary's
containing folder or directory:

@codeblock|{
import-monsters "../monsters.rkt"
}|

One organizational strategy for your bestiaries is the "One Set per File" rule.
Each monster set gets its own bestiary file with an ability deck and monsters.
For example, all the "guard" monsters go with the guard ability deck in
@filepath{guards.rkt}.

Then you might have a final bestiary @filepath{my_bestiary.rkt} that imports
each set, like so:

@filebox["my_bestiary.rkt"]{
@codeblock|{
#lang frosthaven-manager/bestiary

import-monsters "guards.rkt"
}|}

An
@hyperlink["https://github.com/benknoble/frosthaven-manager/tree/main/testfiles/sample-bestiary-import.rkt"]{example
of this format can be found in the source}.

You've now seen everything you need to write your own bestiary. Below,
you'll find a succint reference for the format.

@section{Bestiary Format Reference}

@defmodule[frosthaven-manager/bestiary #:lang]

@margin-note{The @secref{Developer_Reference} for monster information documents the
underlying data structures, like @racket[monster-info], @racket[monster-stats],
and @racket[monster-ability].}

The following terms are used both in the explanation by example:

@itemlist[
          @item{@deftech{Game text} is any non-quote contained in @racket["double quotes"].}
          @item{A @deftech{game number} is a positive or negative number with no extra decoration.}
]

The grammar for the bestiary is as follows. Whitespace is ignored except in
@tech{game text}.

Any bestiary file that defines a monster is required to define an ability deck
for that monster's set. Importing bestiaries that conflict with each other or
with the current bestiary is also an error. Cyclic imports are disallowed.

Any monster's attack or HP values can be specified as formulas, using the
variables @tt{L} and @tt{C} to refer to the current level and number of
characters, respectively. Formulas are written in @tech{game text}, like
@racket["C + 2"] or @racket["L * 3"]. The available operators in formulas are
the standard arithmetic operators @racket["+"], @racket["-"], @racket["*"], and
@racket["/"]. Use parentheses to group expressions, like @racket["(C + 1) * 2"].
The functions @code{up} and @code{down} round up or down and are written
@racket["up(3/2)"] or @racket["down(L/3)"].

Any monster's move value can be specified as "no move value" by writing @tt{-},
like @tt{[Move -]}.

Any @nonterm{card}'s abilities in the @nonterm{text-list} can include a
reference to an AoE pattern by using the function @racket[aoe], like in the
following example: @racket["attack +1, aoe(path/to/triangle.rkt)"]. See
@secref{Area-of-Effect_Specification_by_Example} for more details.

Any @nonterm{card} can refer to elements by writing @racket["Infuse"] or
@racket["Consume"] followed by the name of the element. This is
case-insensitive, so @racket["infuse"] works, too. The element names are
@racket["fire"], @racket["ice"], @racket["air"], @racket["earth"],
@racket["light"], and @racket["dark"] or @racket["darkness"]. You can also write
@racket["Infuse any"] or @racket["Infuse any element"] for wild infusions and
@racket["Consume any"] or @racket["Consume any element"] for wild consumptions.
This is also case-insensitive. In all cases this text will be transformed to a
corresponding pictorial representation.

@(require scribble/bnf)

@BNF[(list @nonterm{bestiary}
           @kleenestar[@BNF-group[@BNF-alt[@nonterm{import} @nonterm{monster} @nonterm{ability deck}]]])

     (list @nonterm{import}
           @BNF-seq[@litchar{import-monsters} @nonterm{file:text}])

     (list @nonterm{monster}
           @BNF-seq-lines[(list @litchar{begin-monster})
                          (list @nonterm{name:text}
                                @optional[@BNF-seq[@litchar{(} @nonterm{set:text} @litchar{)}]])
                          (list @kleenerange[16 16 @nonterm{stats}])
                          (list @litchar{end-monster})])

     (list @nonterm{stats}
           @BNF-seq[@litchar{[} @nonterm{level:number}
                                @BNF-group[@BNF-alt[@litchar{normal} @litchar{elite}]]
                                @nonterm{stat-block} @litchar{]}])

     (list @nonterm{stat-block}
           @BNF-seq[@nonterm{hp} @nonterm{move} @nonterm{attack}
                    @optional{@nonterm{bonuses}}
                    @optional{@nonterm{effects}}
                    @optional{@nonterm{immunities}}])

     (list @nonterm{hp} @BNF-seq[@litchar{[} @litchar{HP}
                                             @BNF-group[@BNF-alt[@nonterm{number} @nonterm{formula:text}]]
                                             @litchar{]}])
     (list @nonterm{move} @BNF-seq[@litchar{[} @litchar{Move}
                                               @BNF-group[@BNF-alt[@nonterm{number} @litchar{-}]]
                                               @litchar{]}])
     (list @nonterm{attack} @BNF-seq[@litchar{[} @litchar{Attack}
                                                 @BNF-group[@BNF-alt[@nonterm{number} @nonterm{formula:text}]]
                                                 @litchar{]}])
     (list @nonterm{bonuses} @BNF-seq[@litchar{[} @litchar{Bonuses} @nonterm{text-list} @litchar{]}])
     (list @nonterm{effects} @BNF-seq[@litchar{[} @litchar{Effects} @nonterm{text-list} @litchar{]}])
     (list @nonterm{immunities} @BNF-seq[@litchar{[} @litchar{Immunities} @nonterm{text-list} @litchar{]}])

     (list @nonterm{ability deck}
           @BNF-seq-lines[(list @litchar{begin-ability-deck})
                          (list @nonterm{set:text})
                          (list @kleenerange[8 8 @nonterm{card}])
                          (list @litchar{end-ability-deck})])

     (list @nonterm{card}
           @BNF-seq[@litchar{[} @litchar{name:text}
                                @litchar{initiative:number}
                                @optional{@litchar{shuffle}}
                                @nonterm{text-list} @litchar{]}])

     (list @nonterm{text-list} @BNF-seq[@litchar["{"] @kleenestar[@nonterm{text}] @litchar["}"]])]

Text like @nonterm{nonterminals} should be replaced with their right-hand-sides.
Text like @litchar{literal} should be typed exactly. Text like
@optional{@nonterm{optional}} is optional. Text like
@kleenestar{@nonterm{repeat}} can be repeated 0 or more times, while
@kleenerange['n 'n]{@nonterm{repeat}} should be repeated exactly @italic{n}
times. Text like @BNF-group[@BNF-alt[@nonterm{one} @nonterm{two}]] is a choice
between each part, separated by bars. Text like @nonterm{label:text} refers to
@tech{game text}, while @nonterm{label:number} refers to an @tech{game number}.

@section{Area-of-Effect Specification by Example}
@defmodule[frosthaven-manager/aoe #:lang]

@(define (aoe-example input)
   (elem
     (spec->shape
       (syntaxes->spec
         (port->list (λ (in) (read-syntax 'in in))
                     (let ([ip (open-input-string input)])
                       (begin0 ip (port-count-lines! ip))))))))

When specifying a monster's abilities, sometimes you need to designate an
Area-of-Effect (AoE, for short). Here we'll describe how to concisely describe
the AoE pattern for use with the monster ability. See
@secref{Bestiary_Format_Reference} for how to refer to an AoE pattern in a
monster ability.

As usual, we'll put the description in a file and start with the language, in
this case @(hash-lang) @racketmodname[frosthaven-manager/aoe]. Then we describe
the hexagonal layout using the symbols @tt{s}, @tt{x}, @tt{o}, @tt{m}, and
@tt{g}. You must put a space between one symbol and the next, as some examples
will show. These symbols correspond to the following hexagons in the resulting
diagram:

@(cc-superimpose
   (rectangle (border-size 5 4) (border-size 5 4))
   (apply vl-append
          20
          (for/list ([ss `((s ,S "Spacer") (x ,X "Attack/Effect") (o ,O "Ally in Position") (m ,M "Me: Creature Activating Ability"))])
            (hc-append 10
              ((cadr ss))
              (text (symbol->string (car ss)))
              (text (caddr ss))))))

The symbol @tt{g} is an invisible "ghost" hex, and is useful when you need to
manually force empty space between hexes. In most cases, however, this should be
taken care of automatically. For example, the following two programs create the
same diagram, shown below:

@codeblock|{
#lang frosthaven-manager/aoe
x g x
}|

and

@codeblock|{
#lang frosthaven-manager/aoe
x   x
}|

both render as follows:

@filebox["x   x"]{@(aoe-example "x   x")}
@filebox["x g x"]{@(aoe-example "x g x")}

Here are some more examples, along with their results.

@filebox["ring1.rkt"]{
@codeblock|{
#lang frosthaven-manager/aoe
 x x
x x x
 x x
}|
}
@(aoe-example " x x\nx x x\n x x")

@filebox["drag-down.rkt"]{
@codeblock|{
#lang frosthaven-manager/aoe
x x
   x
  m
}|
}
@(aoe-example "x x\n   x\n  m")

@filebox["unbreakable-wall.rkt"]{
@codeblock|{
#lang frosthaven-manager/aoe
x x x
 o m
}|
}
@(aoe-example "x x x\n o m")

@filebox["speartip.rkt"]{
@codeblock|{
#lang frosthaven-manager/aoe
   x
  x
 m
o
}|
}
@(aoe-example "   x\n  x\n m\no")

@section{Area-of-Effect Specification Reference}

Space at the beginning of lines is significant, but not at the end. The line
with the left-most character determines which rows are centered and which rows
are offset: alternating rows are always offset relative to each other to create
a hexagonal grid. It is not technically necessary to offset each line in the
textual diagram to achieve correct results, but it is far more readable if you
do so. Thus, we recommend starting alternating lines with either no spaces or 1
space, to create a hex-like effect in the program text.

Recall that all hexagons face North: that is, the top of the hexagon is a point
and not a flat line.

Space must separate hex descriptors to distinguish them.

The following descriptors are available

@(cc-superimpose
   (rectangle (border-size 5 4) (border-size 5 4))
   (apply vl-append
          20
          (for/list ([ss `((s ,S "Spacer") (x ,X "Attack/Effect") (o ,O "Ally in Position") (m ,M "Me: Creature Activating Ability"))])
            (hc-append 10
              ((cadr ss))
              (text (symbol->string (car ss)))
              (text (caddr ss))))))

In addition, the descriptor @tt{g} places an invisible ghost hex; this is
usually not necessary, since spacing takes care of the alignment automatically.

If you have Racket installed, running the programs main module as in
@commandline{racket my-aoe.rkt}
will produce the image.

@section{Foe Specification by Example}

Here's an example foe specification:

@codeblock[#:line-numbers 1]|{
#lang frosthaven-manager/foes

import-monsters "sample-bestiary.rkt"

begin-foe
  "wyrmling archer"
  <[2 absent] [3 normal] [4 elite]>
  <[2 normal] [3 elite] [4 elite]>
end-foe

begin-foe
  "hynox guard" ("guard") (random numbering)
  <[2 elite] [3 elite] [4 elite]>
end-foe
}|

This foe specification imports a sample bestiary and defines two groups of foes
using syntax similar to that of @(hash-lang)
@racketmodname[frosthaven-manager/bestiary]. One is for wyrmling archers and the
other for hynox guards. For each group, we list whether a monster should be
absent, normal, or elite for each of 2, 3, or 4 players. We can optionally
specify how to number the monsters and what set the monster is in, if it cannot
be inferred from the name.

In this example, a 2-player game faces 1 normal wyrmling archer (numbered 1) and
one randomly-numbered elite hynox guard; a 3-player game has one normal and one
elite wyrmling archers (numbered 1 and 2, respectively) and one
randomly-numbered elite hynox guard.

@section{Foe Specification Format Reference}

@defmodule[frosthaven-manager/foes #:lang]

@margin-note{The @secref{Developer_Reference} for monster information documents the
underlying data structures, like @racket[monster-group].}

The grammar for the specification is as follows. Whitespace is ignored except in
@tech{game text}.

Any foe specification file that defines a monster is required to define an
ability deck for that monster's set. Importing bestiaries that conflict with
each other or with the current file is also an error. Cyclic imports are
disallowed.

The default numbering option is ordered.

@(require scribble/bnf)

@BNF[(list @nonterm{foes}
           @kleenestar[@BNF-group[@BNF-alt[@nonterm{import}
                                           @nonterm{monster}
                                           @nonterm{ability deck}
                                           @nonterm{foe}]]])

     (list @nonterm{foe}
           @BNF-seq-lines[(list @litchar{begin-foe})
                          (list @nonterm{name:text}
                                @optional[@BNF-seq[@litchar{(} @nonterm{set:text} @litchar{)}]])
                          (list @optional[@BNF-seq[@litchar{(} @nonterm{how-to-number} @litchar{numbering} @litchar{)}]])
                          (list @kleenerange[1 10 @nonterm{foe-spec}])
                          (list @litchar{end-foe})])

     (list @nonterm{how-to-number}
           @BNF-alt[@litchar{ordered} @litchar{random}])

     (list @nonterm{foe-spec}
           @BNF-seq[@litchar{<}
                     @litchar{[} @litchar{2} @nonterm{foe-type} @litchar{]}
                     @litchar{[} @litchar{3} @nonterm{foe-type} @litchar{]}
                     @litchar{[} @litchar{4} @nonterm{foe-type} @litchar{]}
                    @litchar{>}])

     (list @nonterm{foe-type}
           @BNF-alt[@litchar{absent} @litchar{normal} @litchar{elite}])]

See @secref{Bestiary_Format_Reference} for information on @nonterm{import},
@nonterm{monster}, and @nonterm{ability deck}, as well as how to read this
grammar.

@section{Programming Loot}

@defmodule[frosthaven-manager/loot-cards #:lang]

Like other languages described here, all loot programs start with @(hash-lang)
@racketmodname[frosthaven-manager/loot-cards] in the first line and typically
have a @filepath{.rkt} suffix.

The following commands may be used in
@racketmodname[frosthaven-manager/loot-cards]. The documentation for each
command shows how to use it.

@defform[#:id extend-standard-deck extend-standard-deck]{
This command, written by itself, declares that the following program is based on
the standard set of Frosthaven loot cards. It is mandatory for all loot
programs.

This command may be repeated; each use discards the effects of all prior
commands.
}

@defform[(sticker [stickers card] ...)
         #:grammar ([card (money amount)
                          (material 2p 3p 4p)
                          herb
                          (herb amount)])
         #:contracts ([stickers number?]
                      [amount number?]
                      [2p number?]
                      [3p number?]
                      [4p number?])]{
This command is written parenthesized as shown. It declares that the named
@racket[card]s should have a number of @onscreen{+ 1} stickers added equal to
@racket[stickers]. The @racket[card] must be in the deck.

To specify a card, you write @racket[(money _amount)] for the money card worth a
certain amount. You write @racket[(_material _2p _3p _4p)], where
@racket[_material] is any of @racket[lumber], @racket[hide], or @racket[metal],
for the material card that gives a number of resources for 2 players
(@racket[_2p]), 3 players (@racket[_3p]), and 4 players (@racket[_4p]). You
write @racket[arrowvine], @racket[axenut], @racket[corpsecap],
@racket[flamefruit], @racket[rockroot], or @racket[snowthistle] for the herb
card worth 1 herb. You can also parenthesize and provide an amount for herb
cards worth more.

This command may be repeated.
}

Here is an example loot program that adds 1 sticker to a 2/2/1 lumber card:
@filebox["speartip.rkt"]{
@codeblock|{
#lang frosthaven-manager/loot-cards
extend-standard-deck
(sticker [1 (lumber 2 2 1)])
}|
}
