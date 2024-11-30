#lang racket

(require frosthaven-manager/testfiles/sample-bestiary-import
         frosthaven-manager/defns
         pict
         qi)

(module+ test
  (require rackunit)
  (check-true
   (~> (ability-db) hash-values flatten sep
       (amp (~> monster-ability-abilities flatten sep))
       (any pict?))
   "at least one monster ability has a pict"))
