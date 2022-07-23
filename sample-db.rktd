;; this is all essentially made-up
#s(monster-info "archer"
                "hynox archer"
                (;; 0-7 normal
                 #s(monster-stats 2 2 2 () () ())
                 #s(monster-stats 3 3 3 () () ())
                 #s(monster-stats 4 4 4 () () ())
                 #s(monster-stats 5 5 5 () () ())
                 #s(monster-stats 6 6 6 () () ())
                 #s(monster-stats 7 7 7 () () ())
                 #s(monster-stats 8 8 8 () () ())
                 #s(monster-stats 9 9 9 () () ()))
                (;; 0-7 elite
                 #s(monster-stats 4 2 3 ("shield 1") () ())
                 #s(monster-stats 5 3 4 ("shield 1") () ())
                 #s(monster-stats 6 4 5 ("shield 1") () ())
                 #s(monster-stats 7 5 6 ("shield 2") () ())
                 #s(monster-stats 8 6 7 ("shield 2") () ())
                 #s(monster-stats 9 7 8 ("shield 2") () ())
                 #s(monster-stats 10 8 9 ("shield 3") () ())
                 #s(monster-stats 11 9 10 ("shield 3") () ())))

#s(monster-ability "archer"
                   "double-shot"
                   25
                   ("move +1" "attack +2, range 5" "attack +2, range 5, +1 if same target")
                   #f)
#s(monster-ability "archer"
                   "take aim"
                   80
                   ("move +2" "strengthen self")
                   #t)

;; same type, different
#s(monster-info "archer"
                "wyrmling archer"
                (;; 0-7 normal
                 #s(monster-stats 1 1 1 () () ())
                 #s(monster-stats 2 2 2 () () ())
                 #s(monster-stats 3 3 3 () () ())
                 #s(monster-stats 4 4 4 () () ())
                 #s(monster-stats 5 5 5 () () ())
                 #s(monster-stats 6 6 6 () () ())
                 #s(monster-stats 7 7 7 () () ())
                 #s(monster-stats 8 8 8 () () ()))
                (;; 0-7 elite
                 #s(monster-stats 3 1 2 ("shield 1") () ())
                 #s(monster-stats 4 2 3 ("shield 1") () ())
                 #s(monster-stats 5 3 4 ("shield 1") () ())
                 #s(monster-stats 6 4 5 ("shield 2") () ())
                 #s(monster-stats 7 5 6 ("shield 2") () ())
                 #s(monster-stats 8 6 7 ("shield 2") () ())
                 #s(monster-stats 9 7 8 ("shield 3") () ())
                 #s(monster-stats 10 8 9 ("shield 3") () ())))

;; different type
#s(monster-info "guard"
                "hynox guard"
                (;; 0-7 normal
                 #s(monster-stats 2 2 2 () () ())
                 #s(monster-stats 3 3 3 () () ())
                 #s(monster-stats 4 4 4 () () ())
                 #s(monster-stats 5 5 5 () () ())
                 #s(monster-stats 6 6 6 () () ())
                 #s(monster-stats 7 7 7 () () ())
                 #s(monster-stats 8 8 8 () () ())
                 #s(monster-stats 9 9 9 () () ()))
                (;; 0-7 elite
                 #s(monster-stats 4 2 3 ("shield 1") () ())
                 #s(monster-stats 5 3 4 ("shield 1") () ())
                 #s(monster-stats 6 4 5 ("shield 1") () ())
                 #s(monster-stats 7 5 6 ("shield 2") () ())
                 #s(monster-stats 8 6 7 ("shield 2") () ())
                 #s(monster-stats 9 7 8 ("shield 2") () ())
                 #s(monster-stats 10 8 9 ("shield 3") () ())
                 #s(monster-stats 11 9 10 ("shield 3") () ())))

#s(monster-ability "guard"
                   "rushing charge"
                   25
                   ("move +3" "attack +2 + number of spaces moved towards target")
                   #f)
#s(monster-ability "guard"
                   "stand tall"
                   80
                   ("shield 3")
                   #t)
