#lang frosthaven-manager/bestiary

begin-monster "hynox archer"
  [0 normal [HP  2] [Move 2] [Attack  2]   ]
  [0 elite  [HP  4] [Move 2] [Attack  3] [Bonuses {"shield 1"}] [Effects {"wound"}] ]
  [1 normal [HP  3] [Move 3] [Attack  3]  [Effects {"wound"}] ]
  [1 elite  [HP  5] [Move 3] [Attack  4] [Bonuses {"shield 1"}] [Effects {"wound"}] ]
  [2 normal [HP  4] [Move 4] [Attack  4]  [Effects {"wound"}] ]
  [2 elite  [HP  6] [Move 4] [Attack  5] [Bonuses {"shield 1"}] [Effects {"wound" "stun"}] ]
  [3 normal [HP  5] [Move 5] [Attack  5]  [Effects {"wound" "muddle"}] ]
  [3 elite  [HP  7] [Move 5] [Attack  6] [Bonuses {"shield 2"}] [Effects {"wound" "stun"}] ]
  [4 normal [HP  6] [Move 6] [Attack  6]   ]
  [4 elite  [HP  8] [Move 6] [Attack  7] [Bonuses {"shield 2"}]  ]
  [5 normal [HP  7] [Move 7] [Attack  7]   ]
  [5 elite  [HP  9] [Move 7] [Attack  8] [Bonuses {"shield 2"}]  ]
  [6 normal [HP  8] [Move 8] [Attack  8]   ]
  [6 elite  [HP 10] [Move 8] [Attack  9] [Bonuses {"shield 3"}]  ]
  [7 normal [HP  9] [Move 9] [Attack  9]   ]
  [7 elite  [HP 11] [Move 9] [Attack 10] [Bonuses {"shield 3"}]  ]
end-monster

begin-monster "wyrmling archer"
  [0 normal [HP  1] [Move 1] [Attack 1]   ]
  [0 elite  [HP  3] [Move 1] [Attack 2] [Bonuses {"shield 1"}]  ]
  [1 normal [HP  2] [Move 2] [Attack 2]   ]
  [1 elite  [HP  4] [Move 2] [Attack 3] [Bonuses {"shield 1"}]  ]
  [2 normal [HP  3] [Move 3] [Attack 3]   ]
  [2 elite  [HP  5] [Move 3] [Attack 4] [Bonuses {"shield 1"}]  ]
  [3 normal [HP  4] [Move 4] [Attack 4]   ]
  [3 elite  [HP  6] [Move 4] [Attack 5] [Bonuses {"shield 2"}]  ]
  [4 normal [HP  5] [Move 5] [Attack 5]   ]
  [4 elite  [HP  7] [Move 5] [Attack 6] [Bonuses {"shield 2"}]  ]
  [5 normal [HP  6] [Move 6] [Attack 6]   ]
  [5 elite  [HP  8] [Move 6] [Attack 7] [Bonuses {"shield 2"}]  ]
  [6 normal [HP  7] [Move 7] [Attack 7]   ]
  [6 elite  [HP  9] [Move 7] [Attack 8] [Bonuses {"shield 3"}]  ]
  [7 normal [HP  8] [Move 8] [Attack 8]   ]
  [7 elite  [HP 10] [Move 8] [Attack 9] [Bonuses {"shield 3"}]  ]
end-monster

begin-ability-deck "archer"
  ["double-shot" 25 {"attack +2, range 5" "attack +2, range 3, +1 if same target"}]
  ["double-shot" 25 {"attack +2, range 5" "attack +2, range 3, +1 if same target"}]
  ["double-shot" 25 {"attack +2, range 5" "attack +2, range 3, +1 if same target"}]
  ["explosive" 60 {"attack -1, range 3, aoe(aoes/ring1.rkt)"}]
  ["explosive" 60 {"attack -1, range 3, aoe(aoes/ring1.rkt)"}]
  ["explosive" 60 {"attack -1, range 3, aoe(aoes/ring1.rkt)"}]
  ["explosive" 60 {"attack -1, range 3, aoe(aoes/ring1.rkt)"}]
  ["take aim" 80 shuffle {"move +2" "strengthen self"}]
end-ability-deck
