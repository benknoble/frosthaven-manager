#lang frosthaven-manager/bestiary


begin-ability-deck
    "guard"

    ["rushing charge" 25 {"move +3" "attack +2 + number of spaces moved towards target"}]
    ["rushing charge" 25 {"move +3" "attack +2 + number of spaces moved towards target"}]
    ["rushing charge" 25 {"move +3" "attack +2 + number of spaces moved towards target"}]
    ["rushing charge" 25 {"move +3" "attack +2 + number of spaces moved towards target"}]
    ["rushing charge" 25 {"move +3" "attack +2 + number of spaces moved towards target"}]
    ["rushing charge" 25 {"move +3" "attack +2 + number of spaces moved towards target"}]
    ["rushing charge" 25 {"move +3" "attack +2 + number of spaces moved towards target"}]
    ["stand tall" 80 shuffle {"shield 3"}]
end-ability-deck

begin-monster
    "hynox guard"

    [0 normal [hp 2] [move 2] [attack 2]]
    [1 normal [hp 3] [move 3] [attack 3]]
    [2 normal [hp 4] [move 4] [attack 4]]
    [3 normal [hp 5] [move 5] [attack 5]]
    [4 normal [hp 6] [move 6] [attack 6]]
    [5 normal [hp 7] [move 7] [attack 7]]
    [6 normal [hp 8] [move 8] [attack 8]]
    [7 normal [hp 9] [move 9] [attack 9]]

    [0 elite [hp 4] [move 2] [attack 3] [Bonuses {"shield 1"}]]
    [1 elite [hp 5] [move 3] [attack 4] [Bonuses {"shield 1"}]]
    [2 elite [hp 6] [move 4] [attack 5] [Bonuses {"shield 1"}]]
    [3 elite [hp 7] [move 5] [attack 6] [Bonuses {"shield 2"}]]
    [4 elite [hp 8] [move 6] [attack 7] [Bonuses {"shield 2"}]]
    [5 elite [hp 9] [move 7] [attack 8] [Bonuses {"shield 2"}]]
    [6 elite [hp 10] [move 8] [attack 9] [Bonuses {"shield 3"}]]
    [7 elite [hp 11] [move 9] [attack 10] [Bonuses {"shield 3"}]]
end-monster
