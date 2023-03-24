#lang frosthaven-manager/bestiary

begin-ability-deck
    "boss"

    ["I'm a boss" 45 {"move +1" "attack -1 aoe(aoes/ring1.rkt)"}]
    ["I'm a boss" 45 {"move +1" "attack -1 aoe(aoes/ring1.rkt)"}]
    ["I'm a boss" 45 {"move +1" "attack -1 aoe(aoes/ring1.rkt)"}]
    ["I'm a boss" 45 {"move +1" "attack -1 aoe(aoes/ring1.rkt)"}]
    ["I'm a boss" 45 {"move +1" "attack -1 aoe(aoes/ring1.rkt)"}]
    ["I'm a boss" 45 {"move +1" "attack -1 aoe(aoes/ring1.rkt)"}]
    ["I'm a boss" 45 {"move +1" "attack -1 aoe(aoes/ring1.rkt)"}]
    ["Special" 11 shuffle {"move +1" "attack +3"}]
end-ability-deck

begin-monster
    "giant squid" ("boss")

    [0 normal [hp "C * 10"] [move 3] [attack "C"]]
    [1 normal [hp "C * 12"] [move 3] [attack "C + 1"]]
    [2 normal [hp "C * 15"] [move 3] [attack "C + 1"]]
    [3 normal [hp "C * 17"] [move 3] [attack "C + 2"]]
    [4 normal [hp "C * 22"] [move 3] [attack "C + 2"]]
    [5 normal [hp "C * 25"] [move 3] [attack "C + 3"]]
    [6 normal [hp "C * 35"] [move 3] [attack "C + 4"]]
    [7 normal [hp "C * 35"] [move 3] [attack "C + 5"]]

    [0 elite [hp "C * 10"] [move 3] [attack "C"]]
    [1 elite [hp "C * 12"] [move 3] [attack "C + 1"]]
    [2 elite [hp "C * 15"] [move 3] [attack "C + 1"]]
    [3 elite [hp "C * 17"] [move 3] [attack "C + 2"]]
    [4 elite [hp "C * 22"] [move 3] [attack "C + 2"]]
    [5 elite [hp "C * 25"] [move 3] [attack "C + 3"]]
    [6 elite [hp "C * 35"] [move 3] [attack "C + 4"]]
    [7 elite [hp "C * 35"] [move 3] [attack "C + 5"]]
end-monster
