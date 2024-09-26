#lang scribble/manual

@(require "common.rkt"
          "../elements.rkt"
          (only-in pict vc-append text))

@title{How to play}

At any time, see the @onscreen{Help} menu for more information.

Many of the elements of playing a scenario can be programmed, to be saved and
re-used for later play or to save you the tedium of clicking through screens in
the Frosthaven Manager. See @secref{Programming_a_Scenario} for more details.
Each step that allows loading scenario programs will explicitly mention that in
this guide.

@(define (counter what)
   @elem{Click @onscreen{+} and @onscreen{-} to adjust @|what|.})

@section{Getting started}

When the Frosthave Manager launches, you'll be on the main screen.

Typical first steps include:
@itemlist[
  #:style 'ordered
  @item{@secref{Setting_up_the_players}}
  @item{@secref{Setting_up_the_scenario}}
  @item{@secref{Setting_up_the_monsters}}
  @item{(Optional) @secref{Setting_up_the_loot_deck}}
  @item{(Optional) @secref{Setting_up_round_prompts}}
]

Then its time to @secref{Play_the_scenario}. See @secref{Other_utilities} for
advanced uses that might help with certain game rules.

@section{Setting up the players}

The default game has 2 players. Use the @onscreen{Edit} menu to select
@onscreen{Edit Number of Players} to choose a 3 or 4 player game. Players
are added and removed automatically to match the selection.

Then, for each player, click @onscreen{More Actions} and:
@itemlist[
  #:style 'ordered
  @item{Click @onscreen{Edit Name} to change the player's name}
  @item{Click @onscreen{Change Max. HP} to adjust maximum HP. Current hit points
  are not adjusted, so don't forget to bump those up, too.}
]

If you @seclink["Other_utilities"]{launch the server} after selecting the number
of players in your game, your players can also edit their own name and maximum
health from their web browsers.

@section{Setting up the scenario}

In the same @onscreen{Edit} menu, select @onscreen{Edit Scenario Level} to
choose the scenario level, which is also the default monster level.

@section{Setting up the monsters}

In the @onscreen{Edit} menu, first select @onscreen{Choose Bestiary}. If you try
to @onscreen{Add Monster Group} without choosing a bestiary, you'll first be
asked to choose a bestiary.

@subsection[#:tag select-monster-db-tag]{Choosing a bestiary}

Click either the @onscreen{Open Bestiary or Foes} button or the @onscreen{Use
Default Bestiary} button. Then use the viewer below to explore and confirm the
monster information. If you need to change the bestiary, note that all current
monsters groups will be removed.

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

@subsection{Adding monsters to the scenario}

@margin-note{If you opened a foes specification in
@secref[select-monster-db-tag], you will skip this step.}

Click the @onscreen{Add Monster Group} button to add a monster group to the
scenario. @margin-note*{Adding a monster group can also be trigged from the
@onscreen{Edit} menu with @onscreen{Add Monster Group} or by pressing Ctl-N or
Cmd-N, depending on your operating system.} Choose a set, like
@onscreen{Archer}, and the select a more specific monster, like
@onscreen{Wyrmling Archer}, by using the drop-downs. Then, check the boxes for
the monster numbers you want to include, and check the @onscreen{Elite?} boxes
for any numbers that are elite. Don't forget to adjust the @onscreen{Level}
slider if the monsters in this group are of higher or lower level than normal.
When you're finished, click @onscreen{Add}.

When adding a monster group, you can close the monster group-chooser without
selecting any active numbers to stop adding a new group. Note that you cannot
add two separate groups of the same kind. For example, you can only have one
@onscreen{Wyrmling Archer} group at a time.

@section{Setting up the loot deck}

This step is for those who want to use custom loot cards or who don't want to
play with physical loot cards. Once you set up the loot deck, the
@onscreen{Loot} button will be enabled for both the local application and any
web browsers connected to the @seclink["Other_utilities"]{web server}.

From the @onscreen{Edit} menu, select @onscreen{Build Loot Deck}.
@itemlist[
  @item{To include a ``Random Item'' card, check the box labelled @onscreen{Random Item Card?}.}
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

@section{Setting up round prompts}

You can add a prompt to remind you about a special rule at the beginning or end
of specified rounds. From the @onscreen{Edit} menu, select @onscreen{Manage
Round Prompts}.

A list of prompts is presented. To add a prompt, click @onscreen{Add Prompt} and
choose the timing of a prompt to add. You can remove specific prompts with
@onscreen{X}. These prompts will trigger during the game to remind you to check
the rules. You can add more during the game via the @onscreen{Edit} menu in the
same way.

@section{Play the scenario}

The main scenario screen features several elements, which we will examine in
turn.

@subsection{Elements tracker}

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
button as in @secref{Advancing_the_scenario}.

Here are the elements in each of the unfused, infused, and waning states:

@(apply elem
        (for/list ([e (in-list (elements))])
          (vc-append
            (text (element-pics-name e))
            (element-pics-unfused e)
            (element-pics-infused e)
            (element-pics-waning e))))

@subsection{Monster and round controls}

On the far right are several controls.

@subsubsection{Adding bless and curse}

At the top you will find buttons to add Curse and Bless cards to the monster
decks. They also tell you how many Curse and Bless cards are left to be added.
These buttons will not be active if there are no Curse or Bless cards left.

You will also find buttons to add Bless cards to and from the player decks;
these help track the remaining Bless cards for monsters and have no effect on
players in the Frosthaven Manager. In actual table play, you should expect to
manage the lone Bless deck and keep it in sync with the Frosthaven Manager's
Bless deck. Click @onscreen{Bless Player} when giving a player a Bless; click
@onscreen{Unbless Player} when a Bless comes out of a player deck.

@subsubsection{Drawing from the monster modifier deck}

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

@subsubsection{Advancing the scenario}

At the bottom of the right control group you will find two (2) buttons. Only
one will be available to you at a time. You will also find the round number.

The first button is @onscreen{Next Round}. After every group has taken their
turn, use this button to move to the next round. At this point, you would begin
entering player initiatives to prepare for the new round as in
@secref{Player_controls}. This button also automatically wanes or unfuses active
elements as in @secref{Elements_tracker}.

The second button is @onscreen{Draw Abilities}. Use this button once all player
initiatives are entered to draw an ability card for each monster group and
re-order the creature list by initiative.

Players connected to the @seclink["Other_utilities"]{web server} on their
browsers are notified when rounds start and end via these buttons.

@subsection{Creature list}

In the middle of the screen is the creature list. There is an entry in this list
for each player and for each monster group.

Many items have a @onscreen{More Actions} button that groups advanced actions
for play. You are encouraged to experiment with actions listed there like
creating summons, manipulating health and conditions, and more.

@subsubsection{Player controls}

Each player has the same controls, labelled by the player's name. On the far
left is the initiative control. Click the button to adjust a player's initiative
by entering a numerical value. Close the initiative entry window or press Enter
when you are finished. Each player's initiative is reset to zero (0) when
advancing rounds using the @onscreen{Next Round} button, and the players will be
found at the top of the creature list between rounds. This makes it easy to find
and adjust each player's initiative.

To the right of the initiative controls are the health and experience controls.
@counter{current HP and XP}

Right of the HP and XP controls are conditions. Click @onscreen{Edit Conditions}
to toggle conditions on and off for a player. Close the window of conditions
when finished.

@subsubsection{Monster group controls}

Each monster group has a general statistics panel and an individual monster
panel.

@subsubsub*section{Monster group statistics}

The monster group statistics panel contains the current initiative and ability
if any as well as the Move, Attack, Bonuses, Effects, Immunities, and Max HP
statistics for both normal and elite monsters of that group. Additionally, the
@onscreen{Add Monster} button can be used to add new individual monsters to the
group. Closing the new monster window or clicking @onscreen{Add} will add any
selected monsters.

@subsubsub*section{Individual monsters}

Individual monster controls are similar to player controls. By selecting a
monster number from the row of tabs, you can see the monster number, its current
health, its conditions, and the button to @onscreen{Kill} and remove a monster
from the group. Adjust HP and conditions just like for the players as in
@secref{Player_controls}.

The list of individuals in the tabs shows the monster number, current HP in
parentheses, and an asterisk (@onscreen{*}) if the monster is affected by any
conditions.

Killing the last monster in a group automatically removes that group from play
to simplify the information on display.

@subsection{Scenario information and loot}

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

@section{Saving and loading a game}

At any time, you can save the state of the game using the @onscreen{Save Game}
button in the @onscreen{File} menu. You can also load a saved game using the
@onscreen{Load Game} button in the same menu. See also @secref{Other_utilities}.

@section{Other utilities}

In the @onscreen{File} menu, you will find the following entries:
@itemlist[
  @item{@onscreen{Save Game}: see @secref{Saving_and_loading_a_game}. Also available with Ctl-S or Cmd-S, depending on your operating system.}
  @item{@onscreen{Load Game}: see @secref{Saving_and_loading_a_game}. Also available with Ctl-O or Cmd-O, depending on your operating system.}
  @item{@onscreen{Launch Server}: Launches a local server. Have your players open the address shown in their web browsers and enjoy interacting with the app! Most but not all controls are available in the web browser.}
]

In the @onscreen{Edit} menu, you will find the following in addition to those
described by @secref{Getting_started}:
@itemlist[
  @item{@onscreen{Edit round number}: You can set the round number to whatever you like. Some scenarios do this to make tracking effects easier.}
  @item{@onscreen{Modify Monster Deck}: Allows removing and re-adding cards to the monster modifier deck. This is best done before play begins, but may be done at any time. Use the @onscreen{Shuffle Deck} button mid-scenario to manually reshuffle the deck if needed.}
]

In the @onscreen{Utilities} menu, you will the find following entries:
@itemlist[
  @item{@onscreen{Formula editor}: Evaluates formulas as specified in @secref{Bestiary_Format_Reference} without the @tech{game text} quotes. A frequent use is to calculate scenario level; for example, in a 4-player game writing @tt{up((9 + 8 + 6 + 8)/4/2)} displays the result 4.}
]

In the @onscreen{Debug} menu, you will find the following entries:
@itemlist[
  @item{@onscreen{Observe GC}: This is used by developers to show a visual indicator of @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{garbage collection}. This is not something normal players need to worry about.}
]
