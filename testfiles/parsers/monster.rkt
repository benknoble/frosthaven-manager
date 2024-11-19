#lang racket

(require frosthaven-manager/parsers/monster
         megaparsack)

(define bestiary-bad-formula
  #<<EOF
#lang frosthaven-manager/bestiary

begin-monster "giant squid" ("boss")
  [0 normal [HP "(C * 10"] [Move 3] [Attack     "C"]   ]
  [0 elite  [HP "C * 10"] [Move 3] [Attack     "C"]   ]
  [1 normal [HP "C * 12"] [Move 3] [Attack "C + 1"]   ]
  [1 elite  [HP "C * 12"] [Move 3] [Attack "C + 1"]   ]
  [2 normal [HP "C * 15"] [Move 3] [Attack "C + 1"]   ]
  [2 elite  [HP "C * 15"] [Move 3] [Attack "C + 1"]   ]
  [3 normal [HP "C * 17"] [Move 3] [Attack "C + 2"]   ]
  [3 elite  [HP "C * 17"] [Move 3] [Attack "C + 2"]   ]
  [4 normal [HP "C * 22"] [Move 3] [Attack "C + 2"]   ]
  [4 elite  [HP "C * 22"] [Move 3] [Attack "C + 2"]   ]
  [5 normal [HP "C * 25"] [Move 3] [Attack "C + 3"]   ]
  [5 elite  [HP "C * 25"] [Move 3] [Attack "C + 3"]   ]
  [6 normal [HP "C * 35"] [Move 3] [Attack "C + 4"]   ]
  [6 elite  [HP "C * 35"] [Move 3] [Attack "C + 4"]   ]
  [7 normal [HP "C * 35"] [Move 3] [Attack "C + 5"]   ]
  [7 elite  [HP "C * 35"] [Move 3] [Attack "C + 5"]   ]
end-monster

begin-ability-deck "boss"
  ["I'm a boss" 45 {"move +1" "attack -1 aoe(aoes/ring1.rkt)"}]
  ["I'm a boss" 45 {"move +1" "attack -1 aoe(aoes/ring1.rkt)"}]
  ["I'm a boss" 45 {"move +1" "attack -1 aoe(aoes/ring1.rkt)"}]
  ["I'm a boss" 45 {"move +1" "attack -1 aoe(aoes/ring1.rkt)"}]
  ["I'm a boss" 45 {"move +1" "attack -1 aoe(aoes/ring1.rkt)"}]
  ["I'm a boss" 45 {"move +1" "attack -1 aoe(aoes/ring1.rkt)"}]
  ["I'm a boss" 45 {"move +1" "attack -1 aoe(aoes/ring1.rkt)"}]
  ["Special" 11 shuffle {"move +1" "attack +3"}]
end-ability-deck
EOF
  )

(module+ test
  (require rackunit)
  (check-exn exn:fail:read:megaparsack?
             (thunk
              (parse-bestiary 'bestiary-bad-formula
                              (open-input-string bestiary-bad-formula)
                              #:syntax? #f))))
