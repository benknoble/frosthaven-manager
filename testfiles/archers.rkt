#lang frosthaven-manager/bestiary

begin-ability-deck
    "archer"

    ["double-shot" 25 {"attack +2, range 5" "attack +2, range 3, +1 if same target"}]
    ["double-shot" 25 {"attack +2, range 5" "attack +2, range 3, +1 if same target"}]
    ["double-shot" 25 {"attack +2, range 5" "attack +2, range 3, +1 if same target"}]
    ["explosive" 60 {"attack -1, range 3, aoe(aoes/ring1.rkt)"}]
    ["explosive" 60 {"attack -1, range 3, aoe(aoes/ring1.rkt)"}]
    ["explosive" 60 {"attack -1, range 3, aoe(aoes/ring1.rkt)"}]
    ["explosive" 60 {"attack -1, range 3, aoe(aoes/ring1.rkt)"}]
    ["take aim" 80 shuffle {"move +2" "strengthen self"}]
end-ability-deck

begin-monster
    "hynox archer"

    [0 normal [hp 2] [move 2] [attack 2]]
    [1 normal [hp 3] [move 3] [attack 3]]
    [2 normal [hp 4] [move 4] [attack 4]]
    [3 normal [hp 5] [move 5] [attack 5]]
    [4 normal [hp 6] [move 6] [attack 6]]
    [5 normal [hp 7] [move 7] [attack 7]]
    [6 normal [hp 8] [move 8] [attack 8]]
    [7 normal [hp 9] [move 9] [attack 9]]

    [0 elite [HP 4]  [Move 2] [Attack 3]  [Bonuses {"shield 1"}] [effects {"wound"}]]
    [1 elite [HP 5]  [Move 3] [Attack 4]  [Bonuses {"shield 1"}]]
    [2 elite [HP 6]  [Move 4] [Attack 5]  [Bonuses {"shield 1"}]]
    [3 elite [HP 7]  [Move 5] [Attack 6]  [Bonuses {"shield 2"}]]
    [4 elite [HP 8]  [Move 6] [Attack 7]  [Bonuses {"shield 2"}]]
    [5 elite [HP 9]  [Move 7] [Attack 8]  [Bonuses {"shield 2"}]]
    [6 elite [HP 10] [Move 8] [Attack 9]  [Bonuses {"shield 3"}]]
    [7 elite [HP 11] [Move 9] [Attack 10] [Bonuses {"shield 3"}]]
end-monster

begin-monster
    "wyrmling archer" ("archer")

    [0 normal [hp 1] [move 1] [attack 1]]
    [1 normal [hp 2] [move 2] [attack 2]]
    [2 normal [hp 3] [move 3] [attack 3]]
    [3 normal [hp 4] [move 4] [attack 4]]
    [4 normal [hp 5] [move 5] [attack 5]]
    [5 normal [hp 6] [move 6] [attack 6]]
    [6 normal [hp 7] [move 7] [attack 7]]
    [7 normal [hp 8] [move 8] [attack 8]]

    [0 elite [hp 3] [move 1] [attack 2] [Bonuses {"shield 1"}]]
    [1 elite [hp 4] [move 2] [attack 3] [Bonuses {"shield 1"}]]
    [2 elite [hp 5] [move 3] [attack 4] [Bonuses {"shield 1"}]]
    [3 elite [hp 6] [move 4] [attack 5] [Bonuses {"shield 2"}]]
    [4 elite [hp 7] [move 5] [attack 6] [Bonuses {"shield 2"}]]
    [5 elite [hp 8] [move 6] [attack 7] [Bonuses {"shield 2"}]]
    [6 elite [hp 9] [move 7] [attack 8] [Bonuses {"shield 3"}]]
    [7 elite [hp 10] [move 8] [attack 9] [Bonuses {"shield 3"}]]
end-monster
