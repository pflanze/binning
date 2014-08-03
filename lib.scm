
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

(def. (f64vector.map/iota v f)
  (let* ((len (.length v))
	 (out (make-f64vector len)))
    (for..< (i 0 len)
	    (f64vector-set! out i
			    (f (f64vector-ref v i) i)))
    out))



(def. u32vector.length u32vector-length)

(def. (u32vector.chop-both v)
  (subu32vector v 1 (dec (.length v))))

(TEST
 > (.chop-both (u32vector 0 7 0))
 #u32(7)
 > (.chop-both (u32vector 0 7))
 #u32())




(def. f64vector.ref f64vector-ref)
(def. u8vector.ref u8vector-ref)
(def. s8vector.ref s8vector-ref)
(def. u16vector.ref u16vector-ref)
(def. s16vector.ref s16vector-ref)
(def. s32vector.ref s32vector-ref)
(def. u32vector.ref u32vector-ref)
(def. u64vector.ref u64vector-ref)
(def. s64vector.ref s64vector-ref)
(def. vector.ref vector-ref)
