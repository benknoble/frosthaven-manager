#lang scribble/manual

@(require "common.rkt"
          "../monster-db.rkt"
          racket/file
          (for-label "../defns.rkt"))

@title{Editing Monster Information}

Monster information, including statistics and actions, is stored in a "monster
database." See @secref[select-monster-db-tag] for how the database is loaded into
the game.

@margin-note{Interested in contributing to Frosthaven Manager? Help me build a
monster database editor to make creating custom monster databases easier!}

This section covers where and how to edit a monster database.

@section{Where Can I Store Monster Information?}

Simply: anywhere! Any plain-text file will do.

@margin-note{Note that some "text editors" are for rich text. Do not use
Microsoft Word to edit plain-text files. On macOS, be careful with the
TextEdit.app---it has options to edit rich-text format (RTF) files, which should
be avoided when working with plain text.}

Since the contents of the file will be in a format known as "Racket data," the
traditional suffix is @tt{rktd}. So you might create a plain-text file named
@tt{my_monster_db.rktd} to contain your monster database. We'll cover the exact
format in the next section, @secref{How_can_I_Edit_Monster_Information_}.

@section{How can I Edit Monster Information?}

@subsection{Monster Information Format By Example}

Let's start with an example monster database. This is the one that comes with
Frosthaven Manager for trying things out:

@(typeset-code (file->string default-monster-db)
               #:context #'here
               #:line-numbers 1)

This is a bit long, but we can already identify several distinct components:
@itemlist[#:style 'ordered
          @item{Lines 2, 35, and 57 start definitions for "Monster Info."}
          @item{"Monster Info" contains many "Monster Stats," like on lines 5--12.}
          @item{Lines 23, 28, 78, and 83 start definitions for a "Monster Action."}
]

You might also notice a few other things: lots of parentheses, for one. These
are good! They help us @tech{group} things together. There is also a lot
whitespace to help provide a visual feel of the layout. You are encouraged to
use whitespace for layout, but the Frosthaven Manager will ignore it in most
cases.

@(define ref-doc '(lib "scribblings/reference/reference.scrbl"))
@margin-note{For the adventurous, @tech{game text} obeys all the rules of
@seclink["parse-string" #:doc ref-doc]{Racket strings}. Similarly, numbers can
technically be written in any @seclink["parse-number" #:doc ref-doc]{Racket
number format}.}

You might notice double-quotes @tt{"} around names like @racket["guard"] or
@racket["hynox archer"]. These denote @tech{game text}, which is always enclosed
by double-quotes to group the text together.

You might notice some numbers, like on line 5, are not in quotes. These are
@tech{game numbers}, like max HP. @seclink["parse-number" #:doc ref-doc]{Many
formats will do}, but for Frosthaven Manager you should only see positive
natural numbers, like @racket[1], @racket[2], @racket[3], @racket[42], etc.
Contrast these with numbers written in quotes, like in @racket["shield 3"] on
line 86: these numbers are part of the @tech{game text}, like in an ability.

You might notice some symbols like @tt{#}. The @tt{#s} starts a
@tech{definition}, which is always followed by a @tech{group} whose first
element is the name of the @tech{group}, like @code{#s(monster-info)}. The
@racket[#t] and @racket[#f] are better written as @racket[#true] and
@racket[#false], representing "true" and "false," respectively. These are
@tech{game switch}es.

You might notice lines (like line 1) or parts of lines (like part of line 4)
with semi-colons @tt{;} and english text. These are colored differently because
they are @tech{comments}: they help us humans understand the file, but they are
ignored by the Frosthaven Manager when loading a monster database. This makes
them handy to organize and structure databases.

With these observations, we're ready to write a monster definition!

@subsubsection{Write Your Own Monster}

@; Use codeblock in this section so that the examples look the same as the one
@; rendered by typeset-code above.

We'll start with the monster information, which contains all the statistics
needed for different levels of the monster. To begin, we write

@codeblock|{#s(monster-info )}|

This indicates a monster information @tech{definition}, as observed. Every part
of the information goes @emph{inside the parentheses in a specific order}.

The first two things after the @racket[monster-info] label are the Set name and
Monster name. For example, a Hobgoblin Warrior would have a Set name of
"Warrior" and a Monster name of "Hobgoblin Warrior." Both of these are
@tech{game text}, so don't forget the quotes:

@codeblock|{#s(monster-info "Warrior" "Hobgoblin Warrior")}|

Next comes the normal and elite monster stats, so we need to add two
@tech{group}s. We'll leave them empty for now. Let's add comments and some
space, too, so we know what's what:

@codeblock|{
#s(monster-info "Warrior" "Hobgoblin Warrior"
                ;; Normal stats
                ()
                ;; Elite stats
                ())
}|

Now we need to add the actual stats. These come in groups of eight, one for each
possible monster level. Once you've seen one example of monster stats, you've
seen most of them! We start with a @tech{definition}:

@codeblock|{#s(monster-stats )}|

The first three elements of @racket[monster-stats] are max HP, base movement,
and base attack. These are all @tech{game numbers}. Let's say the Hobgoblin
Warrior has 4 max HP, move 2, and attack 2 at level 0:

@codeblock|{#s(monster-stats 4 2 2)}|

The next three elements of @racket[monster-stats] are persistent bonuses, attack
effects, and condition immunities. All three are @tech{groups} of @tech{game
text}, which means parentheses and quotes. Bonuses are things like "Shield 1."
Attack effects are things like "2 Targets" and "Pierce 1." Condition immunities
are things like "Poison." For many monsters, all three groups will be empty,
written @code{()} like in the long example above. Let's use the examples in this
paragraph for the Hobgoblin Warrior, even though it might be far too strong for
level 0, just to show how to write them:

@codeblock|{
#s(monster-stats 4 2 2
                 ;; persistent bonuses
                 ("Shield 1")
                 ;; attack effects
                 ("2 Targets"
                  "Pierce 1")
                 ;; condition immunities
                 ("Poison"))
}|

Now you've seen it all: we need to write 16 total monster stats for the
Hobgoblin Warrior, which we'll do by making up the numbers as we go. Normally we
would probably refer to an existing Frosthaven monster stats card, but you can
also make your own.

@codeblock|{
#s(monster-info "Warrior" "Hobgoblin Warrior"
                ;; Normal stats
                (#s(monster-stats 4 2 2 () () ())
                 #s(monster-stats 5 2 2 () () ())
                 #s(monster-stats 6 3 3 () () ())
                 #s(monster-stats 7 3 3 () () ())
                 #s(monster-stats 8 3 4 () () ())
                 #s(monster-stats 9 3 4 () () ())
                 #s(monster-stats 10 4 5 () () ())
                 #s(monster-stats 11 4 5 () () ()))
                ;; Elite stats
                (#s(monster-stats 6 3 3 ("Shield 1") ("2 Targets" "Pierce 1") ("Poison"))
                 #s(monster-stats 7 3 3 ("Shield 1") ("2 Targets" "Pierce 1") ("Poison"))
                 #s(monster-stats 8 4 4 ("Shield 2") ("2 Targets" "Pierce 1") ("Poison"))
                 #s(monster-stats 9 4 4 ("Shield 2") ("2 Targets" "Pierce 1") ("Poison"))
                 #s(monster-stats 10 4 5 ("Shield 2") ("2 Targets" "Pierce 1") ("Poison"))
                 #s(monster-stats 11 4 5 ("Shield 2") ("2 Targets" "Pierce 1") ("Poison"))
                 #s(monster-stats 12 5 6 ("Shield 3") ("2 Targets" "Pierce 1") ("Poison"))
                 #s(monster-stats 13 5 6 ("Shield 3") ("2 Targets" "Pierce 1") ("Poison"))))
}|

That's an entire (if poorly-commented) monster definition for Hobgoblin
Warriors. But right now it doesn't have any ability cards!

Let's write an ability card. As usual, we start with a @tech{definition}:

@codeblock|{#s(monster-action )}|

Next we have the Set name to which the ability card belongs. Recall that, for
example, archers all use the same deck. Here we want to use the "Warrior" set.

@codeblock|{#s(monster-action "Warrior")}|

Now we need a name for our card. Let's go with "Crushing Blow."

@codeblock|{#s(monster-action "Warrior" "Crushing Blow")}|

All ability cards have an initiative, so let's fill that in next. I think this
is a slow ability, so let's go with 65:

@codeblock|{#s(monster-action "Warrior" "Crushing Blow" 65)}|

All ability cards have a @tech{group} of abilities, which are @tech{game text}.
I think Crushing Blow should have a short move and a big attack, so lets add two
abilities for that:

@codeblock|{#s(monster-action "Warrior" "Crushing Blow" 65
                              ;; actions on the card:
                              ;; - move
                              ;; - attack with muddle (to crush their spirits)
                              ("Move 2"
                               "Attack +4, Muddle"))}|

The last thing we need is to indicate whether this is a "shuffle" card or
not---recall that some ability cards trigger a re-shuffle of the ability deck
for that set. As usual, if the deck ever runs out, it is also re-shuffled.
Crushing Blow doesn't seem like the kind of ability that needs to shuffle the
ability deck, so we'll indicate the @racket[#false] @tech{game switch}, or no
shuffle:

@codeblock|{#s(monster-action "Warrior" "Crushing Blow" 65
                              ;; actions on the card:
                              ;; - move
                              ;; - attack with muddle (to crush their spirits)
                              ("Move 2"
                               "Attack +4, Muddle")
                              ;; no shuffle
                              #false)}|

If we wanted this card to trigger a shuffle, we would use @racket[#true]
instead.

You've now seen everything you need to write your own monster database. Below,
you'll find a succint reference for the format.

@subsection{Monster Information Format Reference}

@margin-note{The @secref{Developer_Reference} for monster information documents the
underlying data structures, like @racket[monster-info], @racket[monster-stats],
and @racket[monster-action].}

In the reference format below, names in @code{<angle-brackets>} refer to other
rules in the reference, or to the following terms:

@itemlist[
          @item{A @deftech{definition} is a @tech{group} that starts with @tt{#s} and whose first element names what is being defined.}
          @item{A @deftech{group} is contained in @racket[(parentheses)], with elements separated by whitespace.}
          @item{@deftech{Game text} is contained in @racket["double quotes"].}
          @item{A @deftech{game number} is a number with no extra decoration.}
          @item{A @deftech{game switch} is either @racket[#true] or @racket[#false].}
          @item{@deftech{Comments} start with @tt{;} and run to the end of the line.}
]

The use of ellipses @code{...} indicate 0 or more of the preceding item.

@codeblock|{
;; Monster information
#s(monster-info
    ;; Set Name
    <game text>
    ;; Monster Name
    <game text>
    ;; Normal Stats for levels 0-7
    (<monster-stats>
     <monster-stats>
     <monster-stats>
     <monster-stats>
     <monster-stats>
     <monster-stats>
     <monster-stats>
     <monster-stats>)
    ;; Elite Stats for levels 0-7
    (<monster-stats>
     <monster-stats>
     <monster-stats>
     <monster-stats>
     <monster-stats>
     <monster-stats>
     <monster-stats>
     <monster-stats>))

;; Monster Stats
#s(monster-stats
    ;; Max HP
    <game number>
    ;; Base Movement
    <game number>
    ;; Base Attack
    <game number>
    ;; Persistent Bonuses
    (<game text> ...)
    ;; Attack Effects
    (<game text> ...)
    ;; Condition Immunities
    (<game text> ...))

;; Monster Action
#s(monster-action
    ;; Set Name
    <game text>
    ;; Card Name
    <game text>
    ;; Iniative
    <game number>
    ;; Abilities
    (<game text> ...)
    ;; Shuffle the ability deck?
    <game switch>)
}|
