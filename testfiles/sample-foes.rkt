#lang frosthaven-manager/foes

import-monsters "sample-bestiary-import.rkt"

begin-foe
  "wyrmling archer"
  <[2 absent] [3 normal] [4 elite]>
  <[2 normal] [3 elite] [4 elite]>
end-foe

begin-foe
  "hynox guard" ("guard") (random numbering)
  <[2 elite] [3 elite] [4 elite]>
end-foe
