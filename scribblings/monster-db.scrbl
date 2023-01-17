#lang scribble/manual

@(require "common.rkt"
          "../monster-db.rkt"
          racket/file
          (for-label frosthaven-manager/defns))

@title{Editing Monster Information}

Monster information, including statistics and abilities is stored in a
"bestiary." See @secref[select-monster-db-tag] for how the bestiary is loaded
into the game.

@margin-note{Interested in contributing to Frosthaven Manager? Help me build a
bestiary editor to make creating custom bestiaries easier!}

This section covers where and how to edit a bestiary.

@section{Where Can I Store Monster Information?}

Simply: anywhere! Any plain-text file will do.

@margin-note{Note that some "text editors" are for rich text. Do not use
Microsoft Word to edit plain-text files. On macOS, be careful with the
TextEdit.app---it has options to edit rich-text format (RTF) files, which should
be avoided when working with plain text.}

Since the contents of the file will be a program in a Racket dialect, the
traditional suffix is @tt{rkt}. So you might create a plain-text file named
@tt{my_bestiary.rkt} to contain your bestiary. We'll cover the exact
format in the next sections, starting with @secref{Monster_Information_Format_by_Example}.

Instead of a bestiary, you can use a foe specification. These tend to be
scenario-specific, rather than a general collection of monsters. You can use all
the things you learn about bestiaries to define or import monsters and
abilities, and you will use a few new things to specify what foes you'll face in
the scenario. A good pattern for personal organization is to use bestiaries to
define a collection of monsters, and foe specifications to define
scenario-specific foes by importing the bestiary. We'll cover this part starting
in @secref{Foe_Specification_by_Example}.

@section{Monster Information Format by Example}

Let's start with an example bestiary. This is the one that comes with
Frosthaven Manager to try things out:

@(typeset-code (file->string default-monster-db)
               #:context #'here
               #:line-numbers 1)

This is a bit long, but we can already identify several distinct components:
@itemlist[#:style 'ordered
          @item{Line 1 marks the file as containing a bestiary.}
          @item{Lines 2, 38, and 60 begin monsters.}
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

Use the @tt{/} slash character to separate folders and directories from
filenames. If there is no slash, the bestiary from which to import is assumed to
be in the same folder or directory as the bestiary containing the import
command. Use @tt{..} to mean the folder or directory containing the bestiary
with the import command, so that the following command imports
@filepath{monsters.rkt} in the current bestiary's containing folder or
directory:

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

An @hyperlink["https://github.com/benknoble/frosthaven-manager/tree/main/testfiles/sample-bestiary-import.rkt"]{example of this format can be found in the source}.

You've now seen everything you need to write your own bestiary. Below,
you'll find a succint reference for the format.

@section{Monster Information Format Reference}

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

     (list @nonterm{hp} @BNF-seq[@litchar{[} @litchar{HP} @nonterm{number} @litchar{]}])
     (list @nonterm{move} @BNF-seq[@litchar{[} @litchar{Move} @nonterm{number} @litchar{]}])
     (list @nonterm{attack} @BNF-seq[@litchar{[} @litchar{Attack} @nonterm{number} @litchar{]}])
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
                          (list @kleenerange[0 10 @nonterm{foe-spec}])
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
           @BNF-alt[@litchar{absent} @litchar{normal} @litchar{elite}])

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

     (list @nonterm{hp} @BNF-seq[@litchar{[} @litchar{HP} @nonterm{number} @litchar{]}])
     (list @nonterm{move} @BNF-seq[@litchar{[} @litchar{Move} @nonterm{number} @litchar{]}])
     (list @nonterm{attack} @BNF-seq[@litchar{[} @litchar{Attack} @nonterm{number} @litchar{]}])
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
