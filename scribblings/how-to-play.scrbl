#lang scribble/manual

@(require "common.rkt"
          "../elements.rkt"
          (only-in pict vc-append text))

@title{How to Play}

At any time, see the @onscreen{Help} menu for more information.

Many of the elements of playing a scenario can be programmed, to be saved and
re-used for later play or to save you the tedium of clicking through screens in
the Frosthaven Manager. See @secref{Programming_a_Scenario} for more details.
Each step that allows loading scenario programs will explicitly mention that in
this guide.

@(define step
   (let ([n (box 1)])
     (λ (name #:tag [tag #f])
       (begin0
         (if tag
           (section #:tag tag (format "Step ~a: ~a" (unbox n) name))
           (section (format "Step ~a: ~a" (unbox n) name)))
         (set-box! n (add1 (unbox n)))))))

@(define next @elem{Then click @onscreen{Next}.})

@(define (counter what)
   @elem{Click @onscreen{+} and @onscreen{-} to adjust @|what|.})

@step{Get Started}

When the Frosthave Manager launches, you'll pick a scenario level and number of
players from the drop-downs available.
@next

@step{Enter Player Information}

On the next screen, enter the names of the players, and adjust their maximum
health with the plus and minus buttons. @counter{each player's maximum HP}
@next

@step{Build the Loot Deck}

On the next screen, build the loot deck:
@itemlist[
          @item{To include a "Random Item" card, check the box labelled @onscreen{Random Item Card?}.}
          @item{To include loot cards worth gold, adjust the number of money cards. @counter{the number of money cards in the deck}}
          @item{To include materials, increase or decrease the number of each material card. @counter{the number of each material card in the deck}}
          @item{Similarly for herbs: @counter{the number of each herb card in the deck}}
]

You can browse the list of possible loot cards on the left of the screen. You
can also reset the deck to the standard configuration or load a custom deck
using the provided controls. See @secref{Programming_Loot} to write loot
programs. Double-clicking a loot card will add a @onscreen{+ 1} sticker to it.
There is no facility to remove stickers; instead, reset to the standard
configuration or reload your custom deck to remove all stickers.

@next

@step{Add Round Prompts}

You can add a prompt to execute a special rule at the beginning or end of
specified rounds by clicking @onscreen{Add Prompt} and choosing the timing of
a prompt to add. You can remove specific prompts with @onscreen{X}. These
prompts will trigger during the game to remind you to check the rules. You can
add more during the game via the @onscreen{Edit} menu.

@next

@step[#:tag select-monster-db-tag]{Select a Bestiary}

Click either the @onscreen{Open Bestiary or Foes} button or the @onscreen{Use
Default Bestiary} button. Then use the viewer below to explore and confirm the
monster information. @next

Opening a bestiary allows you to completely customize monster statistics and
behavior so that you can enter the monsters needed for a specific scenario or
game. Instead of a bestiary, you can open a foes specification which bundles a
bestiary with a specification of which monsters will be present, normal or
elite, for this scenario.

Using the default bestiary loads a demo database that is useful for trying out
Frosthaven Manager.

In a real game of Frosthaven, you likely want to use a custom bestiary. See
@secref{Programming_a_Scenario} for more information on how to create a custom
bestiary. @margin-note{Interested in contributing to Frosthaven Manager? Help me
build a bestiary editor to make creating custom bestiaries easier!}

@step{Add Monsters to the Scenario}

@margin-note{If you opened a foes specification in
@secref[select-monster-db-tag], you will skip this step.}

Click the @onscreen{Add Monster} button to add a monster group to the scenario.
Choose a set, like @onscreen{Archer}, and the select a more specific monster,
like @onscreen{Wyrmling Archer}, by using the drop-downs. Then, check the boxes
for the monster numbers you want to include, and check the @onscreen{Elite?}
boxes for any numbers that are elite. Don't forget to adjust the
@onscreen{Level} slider if the monsters in this group are of higher or lower
level than normal. When you're finished, click @onscreen{Add}.

The new monster group will appear in the list of monster groups for the
scenario, along with a short table summary. You can click the @onscreen{Remove}
button to delete that monster group from the scenario.

When adding a monster group, you can close the monster group-chooser without
selecting any active numbers to stop adding a new group. Note that you cannot
add two separate groups of the same kind. For example, you can only have one
@onscreen{Wyrmling Archer} group at a time.

When you're finished adding monster groups, click @onscreen{Next}.

@step{Play the Scenario}

After adding monster groups, you'll reach the main scenario screen. It features
several elements, which we will examine in turn.

@subsection{Elements Tracker}

On the far left is the elements tracker. Each of the six (6) elements is
represented by a labelled icon and a button. The button controls the state of
the element: click to infuse an element, and notice that the background of the
element fills up with color. Click again to manually wane the element; notice
the background is only half full. Click a third time to completely unfuse the
element---now, the background should be back to black. Alternately, you can
click the element to transition to the state indicated by the button. You can
also right-click the element to transition to any state.

The @onscreen{Infuse All} and @onscreen{Consume All} buttons will infuse or
unfuse all the elements, respectively.

Note that elements are advanced automatically by the @onscreen{Next Round}
button as in @secref{Advancing_the_Scenario}.

Here are the elements in each of the unfused, infused, and waning states:

@(apply elem
        (for/list ([e (in-list (elements))])
          (vc-append
            (text (element-pics-name e))
            (element-pics-unfused e)
            (element-pics-infused e)
            (element-pics-waning e))))

@subsection{Monster and Round Controls}

On the far right are several controls.

@subsubsection{Adding Bless and Curse}

At the top you will find buttons to add Curse and Bless cards to the monster
decks. They also tell you how many Curse and Bless cards are left to be added.
These buttons will not be active if there are no Curse or Bless cards left.

You will also find buttons to add Bless cards to and from the player decks;
these help track the remaining Bless cards for monsters and have no effect on
players in the Frosthaven Manager. In actual table play, you should expect to
manage the lone Bless deck and keep it in sync with the Frosthaven Manager's
Bless deck. Click @onscreen{Bless Player} when giving a player a Bless; click
@onscreen{Unbless Player} when a bless comes out of a player deck.

@subsubsection{Drawing from the Monster Modifier Deck}

Towards the middle of the right group of controls you will find a group of
controls for the monster's modifier deck. Three (3) buttons draw cards from the
modifier deck. The first says @onscreen{Draw Modifier} and indicates how many
modifier cards are in the deck. It draws a single card, which is shown next on
the Discard Pile. As you draw more cards, only the top of the discard pile is
shown. The next modifier-drawing button reads @onscreen{Advantage}. This button
draws two modifier cards and uses the better of the two, like drawing with
advantage. @emph{The final card will always appear on top of the discard pile.}
The third and final modifier card button is @onscreen{Disadvantage}. Like the
@onscreen{Advantage} button, two cards are drawn. This time, the worse of the
two is used, and the better is discarded. The used card will appear on top of
the discard pile.

You can also click @onscreen{Show Discard} to see the entire discard pile with
the most recently discarded cards on top.

@subsubsection{Advancing the Scenario}

At the bottom of the right control group you will find two (2) buttons. Only
one will be available to you at a time. You will also find the round number.

The first button is @onscreen{Next Round}. After every group has taken their
turn, use this button to move to the next round. At this point, you would begin
entering player initiatives to prepare for the new round as in
@secref{Player_Controls}. This button also automatically wanes or unfuses active
elements as in @secref{Elements_Tracker}.

The second button is @onscreen{Draw Abilities}. Use this button once all player
initiatives are entered to draw an ability card for each monster group and
re-order the creature list by initiative.

@subsection{Creature List}

In the middle of the screen is the creature list. There is an entry in this list
for each player and for each monster group.

Many items have a @onscreen{More Actions} button that groups advanced actions
for play. You are encouraged to experiment with actions listed there like
creating summons, manipulating health and conditions, and more.

@subsubsection{Player Controls}

Each player has the same controls, labelled by the player's name. On the far
left is the initiative control. Click the button to adjust a player's initiative
with the slider. Close the window with the slider when you are finished. Each
player's initiative is reset to zero (0) when advancing rounds using the
@onscreen{Next Round} button, and the players will be found at the top of the
creature list between rounds. This makes it easy to find and adjust each
player's initiative.

To the right of the initiative controls are the health and experience controls.
@counter{current HP and XP}

Right of the HP and XP controls are conditions. Click @onscreen{Edit Conditions}
to toggle conditions on and off for a player. Close the window of conditions
when finished.

@subsubsection{Monster Group Controls}

Each monster group has a general statistics panel and an individual monster
panel.

@subsubsub*section{Monster Group Statistics}

The monster group statistics panel contains the current initiative and ability
if any as well as the Move, Attack, Bonuses, Effects, Immunities, and Max HP
statistics for both normal and elite monsters of that group. Additionally, the
@onscreen{Add Monster} button can be used to add a new individual monster to the
group. At time of writing, closing the new monster window will add it; to cancel
a new individual monster, add it and the kill it as described below.

@subsubsub*section{Individual Monsters}

Individual monster controls are similar to player controls. By selecting a
monster number from the row of tabs, you can see the monster number, its current
health, its conditions, and the button to @onscreen{Kill} and remove a monster
from the group. Adjust HP and conditions just like for the players as in
@secref{Player_Controls}.

The list of individuals in the tabs shows the monster number, current HP in
parentheses, and an asterisk (*) if the monster is affected by any conditions.

@subsection{Scenario Information and Loot}

Last but not least, at the bottom of the screen lie loot and information
controls.

Click @onscreen{Show Loot and XP} to show all loot and XP for every player. This
is especially useful when a scenario is completed.

Click the @onscreen{Loot!} button to draw a loot card; in the window that
appears, click the name of a player to assign that loot card to the selected
player. You can also place the card on the top or bottom of the loot deck.

The @onscreen{Level Stats} panel shows current Trap and Hazardous Terrain
damage, the value of Gold, and XP and Inspiration bonuses for completing the
scenario. The @onscreen{Level Table} and @onscreen{Inspiration Table} buttons
show tables for the scenario statistics by level and inspiration by number of
players, respectively.

And that's it! Now you know how to play!

@section{Saving and Loading a Game}

At any time, you can save the state of the game using the @onscreen{Save Game}
button in the @onscreen{File} menu. You can also load a saved game using the
@onscreen{Load Game} button in the same menu.

@section{Other Utilities}

In the @onscreen{File} menu, you will also find the following utilities:
@itemlist[
    @item{Launch server. Launches a local server. Have your players open the address shown in their web browsers and enjoy interacting with the app!}
]

In the @onscreen{Edit} menu, you will also find:
@itemlist[
    @item{Edit Level. Allows editing the scenario level.}
    @item{Formula editor. Evaluates formulas as specified in @secref{Bestiary_Format_Reference} without the @tech{game text} quotes.}
    @item{Manage Prompts. Permits adding and removing round prompts.}
    @item{@onscreen{Modify Monster Deck}. Allows removing and re-adding cards to
    the monster modifier deck. This is best done before play begins, but may be
    done at any time. Use the @onscreen{Shuffle Deck} button mid-scenario to
    manually reshuffle the deck if needed.}
]
