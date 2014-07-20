
(def (integer-average a b)
     (arithmetic-shift (+ a b) -1))

(TEST
 > (integer-average 1 1)
 1
 > (integer-average 1 2)
 1
 > (integer-average 1 3)
 2
 > (integer-average 0 3)
 1
 > (integer-average 3 0)
 1
 > (integer-average 3 -4)
 -1)


(def. f64vector.length f64vector-length)
(def. f64vector.list f64vector->list)
(def. list.f64vector list->f64vector)
(def inexact exact->inexact)

(def inexact-real? (both real? inexact?))
