#lang scribble/manual

@(require "common.rkt"
          "../elements.rkt"
          (only-in pict vc-append text))

@title{How to Play}

@(define step
   (let ([n (box 1)])
     (λ (name #:tag [tag #f])
       (begin0
         (if tag
           (section #:tag tag (format "Step ~a: ~a" (unbox n) name))
           (section (format "Step ~a: ~a" (unbox n) name)))
         (set-box! n (add1 (unbox n)))))))

@define[next]{Then click "Next."}

@(define (counter what)
   @elem{Click "+" and "-" to adjust @|what|.})

@step{Get Started}

When the Frosthave Manager launches, you'll pick a scenario level and number of
players from the drop-downs available.
@next

@step{Enter Player Information}

On the next screen, enter the names of the players, and adjust their maximum
health with the plus and minus buttons. @counter{each player's maximum HP}
@next

@step{Build the Loot Deck}

@margin-note{At time of writing, this step does not yet support modified loot cards.}

On the next screen, build the loot deck:
@itemlist[
          @item{To include a "Random Item" card, check the box labelled "Random Item Card?".}
          @item{To include loot cards worth gold, adjust the number of money cards. @counter{the number of money cards in the deck}}
          @item{To include materials, increase or decrease the number of each material card. @counter{the number of each material card in the deck}}
          @item{Similarly for herbs: @counter{the number of each herb card in the deck}}
]
@next

@step[#:tag select-monster-db-tag]{Select a Bestiary}

Click either the "Open Monster DB" button or the "Use Default Monster DB"
button. Then use the viewer below to explore and confirm the monster
information. @next

Opening a bestiary allows you to completely customize monster statistics and
behavior so that you can enter the monsters needed for a specific scenario or
game.

Using the default bestiary loads a demo database that is useful for trying out
Frosthaven Manager. In a real game of Frosthaven, you likely want to use a
custom bestiary. See @secref{Editing_Monster_Information} for more information
on how to create a custom bestiary. @margin-note{Interested in contributing to
Frosthaven Manager? Help me build a bestiary editor to make creating custom
bestiaries easier!}

@step{Add Monsters to the Scenario}

Click the "Add Monster" button to add a monster group to the scenario. Choose a
set, like "Archer," and the select a more specific monster, like "Wyrmling
Archer," by using the drop-downs. Then, check the boxes for the monster numbers
you want to include, and check the "Elite?" boxes for any numbers that are
elite. Don't forget to adjust the "Level" slider if the monsters in this group
are of higher or lower level than normal. When you're finished, click "Add."

The new monster group will appear in the list of monster groups for the
scenario, along with a short table summary. You can click the "Remove" button to
delete that monster group from the scenario.

When adding a monster group, you can close the monster group-chooser without
selecting any active numbers to stop adding a new group. Note that you cannot
add two separate groups of the same kind. For example, you can only have one
"Wyrmling Archer" group at a time.

When you're finished adding monster groups, click "Next."

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

The "Infuse All" and "Consume All" buttons will infuse or unfuse all the
elements, respectively.

Note that elements are advanced automatically by the "Next Round" button as in
@secref{Advancing_the_Scenario}.

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

@subsubsection{Drawing from the Monster Modifier Deck}

Towards the middle of the right group of controls you will find a group of
controls for the monster's modifier deck. Three (3) buttons draw cards from the
modifier deck. The first says "Draw Modifier" and indicates how many modifier
cards are in the deck. It draws a single card, which is shown next to "Most
Recent Modifier." As you draw more cards, only the most recent card (currently
used) and the previous card are shown. The next modifier-drawing button reads
"Advantage." This button draws two modifier cards and uses the better of the
two, like drawing with advantage. @emph{The final card will always appear next
to "Most Recent Modifier."} You can see the discarded, worse card as the
"Previous Modifier." The third and final modifier card button is "Disadvantage."
Like the "Advantage" button, two cards are drawn. This time, the worse of the
two is used, and the better is discarded. The used card will appear next to
"Most Recent Modifier," with the better next to "Previous Modifier."

You can also click "Show Discard Pile" to see the entire discard pile with the
most recently discarded cards on top.

@subsubsection{Advancing the Scenario}

At the bottom of the right control group you will find two (2) buttons. Only
one will be available to you at a time. You will also find the round number.

The first button is "Next Round." After every group has taken their turn, use
this button to move to the next round. At this point, you would begin entering
player initiatives to prepare for the new round as in @secref{Player_Controls}.
This button also automatically wanes or unfuses active elements as in
@secref{Elements_Tracker}.

The second button is "Draw Abilities." Use this button once all player
initiatives are entered to draw an ability card for each monster group and
re-order the creature list by initiative.

@subsection{Creature List}

In the middle of the screen is the creature list. There is an entry in this list
for each player and for each monster group.

@subsubsection{Player Controls}

Each player has the same controls, labelled by the player's name. On the far
left is the initiative control. Click the button to adjust a player's initiative
with the slider. Close the window with the slider when you are finished. Each
player's initiative is reset to zero (0) when advancing rounds using the "Next
Round" button, and the players will be found at the top of the creature list
between rounds. This makes it easy to find and adjust each player's initiative.

To the right of the initiative controls are the health and experience controls.
@counter{current HP and XP}

Right of the HP and XP controls are conditions and loot. Click "Edit Conditions"
to toggle conditions on and off for a player. Close the window of conditions
when finished. Click "Show Loot" to show all loot a player currently has. This
is especially useful when a scenario is completed.

@subsubsection{Monster Group Controls}

Each monster group has a general statistics panel and an individual monster
panel.

@subsubsub*section{Monster Group Statistics}

The monster group statistics panel contains the current initiative and ability
if any as well as the Move, Attack, Bonuses, Effects, Immunities, and Max HP
statistics for both normal and elite monsters of that group. Additionally, the
"Add Monster" button can be used to add a new individual monster to the group.
At time of writing, closing the new monster window will add it; to cancel a new
individual monster, add it and the kill it as described below.

@subsubsub*section{Individual Monsters}

Individual monster controls are similar to player controls. By selecting a
monster number from the row of tabs, you can see the monster number, its current
health, its conditions, and the button to "Kill" and remove a monster from the
group. Adjust HP and conditions just like for the players as in
@secref{Player_Controls}.

The list of individuals in the tabs shows the monster number, current HP in
parentheses, and an asterisk (*) if the monster is affected by any conditions.

@subsection{Scenario Information and Loot}

Last but not least, at the bottom of the screen lie loot and information
controls.

Click the "Loot!" button to draw a loot card; in the window that appears, click
the name of a player to assign that loot card to the selected player. Close the
window to assign the card to no player.

The "Level Stats" panel shows current Trap and Hazardous Terrain damage, the
value of Gold, and XP and Inspiration bonuses for completing the scenario. The
"Level Table" and "Inspiration Table" buttons show tables for the scenario
statistics by level and inspiration by number of players, respectively.

@subsection{Saving and Loading a Game}

At any time, you can save the state of the game using the @onscreen{Save Game}
button in the @onscreen{File} menu. You can also load a saved game using the
@onscreen{Load Game} button in the same menu.

And that's it! Now you know how to play!
