#lang frosthaven-manager/bestiary

begin-monster "hynox guard"
  [0 normal [HP 2] [Move 2] [Attack  2]   ]
  [0 elite  [HP 2] [Move 2] [Attack  3] [Bonuses {"shield 1"}]  ]
  [1 normal [HP 3] [Move 3] [Attack  3]   ]
  [1 elite  [HP 3] [Move 3] [Attack  4] [Bonuses {"shield 1"}]  ]
  [2 normal [HP 4] [Move 4] [Attack  4]   ]
  [2 elite  [HP 4] [Move 4] [Attack  5] [Bonuses {"shield 1"}]  ]
  [3 normal [HP 5] [Move 5] [Attack  5]   ]
  [3 elite  [HP 5] [Move 5] [Attack  6] [Bonuses {"shield 2"}]  ]
  [4 normal [HP 6] [Move 6] [Attack  6]   ]
  [4 elite  [HP 6] [Move 6] [Attack  7] [Bonuses {"shield 2"}]  ]
  [5 normal [HP 7] [Move 7] [Attack  7]   ]
  [5 elite  [HP 7] [Move 7] [Attack  8] [Bonuses {"shield 2"}]  ]
  [6 normal [HP 8] [Move 8] [Attack  8]   ]
  [6 elite  [HP 8] [Move 8] [Attack  9] [Bonuses {"shield 3"}]  ]
  [7 normal [HP 9] [Move 9] [Attack  9]   ]
  [7 elite  [HP 9] [Move 9] [Attack 10] [Bonuses {"shield 3"}]  ]
end-monster

begin-ability-deck "guard"
  ["rushing charge" 25 {"move +3" "attack +2 + number of spaces moved towards target"}]
  ["rushing charge" 25 {"move +3" "attack +2 + number of spaces moved towards target"}]
  ["rushing charge" 25 {"move +3" "attack +2 + number of spaces moved towards target"}]
  ["rushing charge" 25 {"move +3" "attack +2 + number of spaces moved towards target"}]
  ["rushing charge" 25 {"move +3" "attack +2 + number of spaces moved towards target"}]
  ["rushing charge" 25 {"move +3" "attack +2 + number of spaces moved towards target"}]
  ["rushing charge" 25 {"move +3" "attack +2 + number of spaces moved towards target"}]
  ["stand tall" 80 shuffle {"shield 3"}]
end-ability-deck
