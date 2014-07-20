(declare (standard-bindings) (extended-bindings))

;; XX lib

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

;; /lib


(def rs (make-random-source))
(random-source-randomize! rs)

(def random-f64vector (random-source-make-f64vectors rs))

(def (binnums n)
     (.f64vector
      (map/iota (lambda (v i)
		  (inexact (* v (/ i n))))
		(.list (random-f64vector n)))))


(def (bin vec numbuckets)
     (let* ((len (.length vec))
	    (res (make-u32vector numbuckets))
	    (numbuckets* (inexact numbuckets)))
       ;;(declare (not safe))
       (time
	(for..< (i 0 len)
		(let* ((v (f64vector-ref vec i))
		       (i* (inexact->exact
			    (floor
			     (* v numbuckets*)))))
		  (u32vector-set! res i*
				  (+ (u32vector-ref res i*) 1)))))
       res))

(def (bin* vec numbuckets)
     (let* ((len (.length vec))
	    (res (make-u32vector numbuckets))
	    (numbuckets* (inexact numbuckets)))
       (declare (not safe))
       (time
	(##c-code "
int len= ___INT(___ARG1);
int numbuckets= ___INT(___ARG2);
double* vec= ___BODY(___ARG3);
unsigned int* res= ___BODY(___ARG4);
int i;

for (i=0; i<len; i++) {
    int ii= vec[i] * numbuckets;
    res[ii]++;
}
" len numbuckets vec res))
       res))


;; uneven buckets ?

(def (bins:search val buckets nbuckets)
     (let lp ((lo 0)
	      (hi nbuckets))
       (if (< lo hi)
	   (let ((mid (integer-average lo hi)))
	     (if (< val (f64vector-ref buckets mid))
		 (lp lo mid)
		 (lp (inc mid) hi)))
	   lo)))

(TEST
 > (def (t x)
	(bins:search x (f64vector 0.0 0.5 1.0) 3))
 > (t 0.0)
 1
 > (t 0.4)
 1
 > (t 0.5)
 2
 > (t 0.9)
 2
 > (t 1.0)
 3
 > (t 1.5)
 3
 > (t -1.0)
 0)

(def (bins vals buckets)
     (let ((nvals (.length vals))
	   (nbuckets (.length buckets)))
       (let ((res (make-u32vector (inc nbuckets))))
	 (time
	  (for..<
	   (i 0 nvals)
	   (let* ((val (f64vector-ref vals i))
		  (bi (bins:search val buckets nbuckets)))
	     (u32vector-set! res bi
			     (+ (u32vector-ref res bi) 1)))))
	 res)))

(TEST
 > (bins (f64vector 0.1 0.4 0.0) (f64vector 0.0 0.5 1.0))
 #u32(0 3 0 0)
 > (bins (f64vector 0.3 0.4 0.0 1.0) (f64vector 0.2 0.5 1.0))
 #u32(1 2 0 1)
 )

(def (gen-buckets n lo hi)
     (let* ((d (- hi lo))
	    (res (make-f64vector (inc n))))
       (for..< (i 0 (inc n))
	       (f64vector-set! res i (+ lo (* d (/ i n)))))
       res))

(TEST
 > (gen-buckets 10 0. 1.)
 #f64(0. .1 .2 .3 .4 .5 .6 .7 .8 .9 1.))


(def (histogram-strip-overflows v)
     (let ((len (u32vector-length v)))
       (subu32vector v 1 (dec len))))

(TEST
 > (histogram-strip-overflows (u32vector 0 7 0))
 #u32(7))

(TEST
 > (def nums (random-f64vector 1000000))
 > (def buckets (gen-buckets 1000 0. 1.))
 > (equal? (histogram-strip-overflows (bins nums buckets))
	   (bin nums 1000))
 #t)


(def (bins* vals buckets)
     (let ((nvals (.length vals))
	   (nbuckets (.length buckets)))
       (let ((res (make-u32vector (inc nbuckets))))
	 (time
	  (##c-code "
int nvals= ___INT(___ARG1);
double* vals= ___BODY(___ARG2);
int nbuckets= ___INT(___ARG3);
double* buckets= ___BODY(___ARG4);
unsigned int* res= ___BODY(___ARG5);

int i;
for (i=0; i<nvals; i++) {
    double val= vals[i];
    int bi;
    {
        int lo=0, hi=nbuckets;
        while (lo < hi) {
            int mid= (lo+hi) / 2;
            if (val < buckets[mid]) {
                hi= mid;
            } else {
                lo= mid+1;
            }
        }
        bi= lo;
    }
    res[bi]++;
}
"
		    nvals
		    vals
		    nbuckets
		    buckets
		    res))
	 res)))

(TEST
 > (equal? (histogram-strip-overflows (bins* nums buckets))
	   (bin nums 1000))
 #t)

