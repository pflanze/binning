;; Program to see how long it takes to build histograms from 1 million
;; floating point values when using a standard CPU.  See "I don't
;; understand why binning 1 million values would take more than a
;; second on the CPU" on https://news.ycombinator.com/item?id=7893521

;; Since the publication above, use of OpenMP has been added. For the
;; new timings on the same machine, see commit log.

(declare (standard-bindings) (extended-bindings))

(include "lib.scm")

(def rs (make-random-source))
(random-source-randomize! rs)

(def random-f64vector (random-source-make-f64vectors rs))

(def (binnums n)
     (.map/iota (random-f64vector n)
		(lambda (v i)
		  (inexact (* v (/ i n))))))


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

(def (gen-buckets #(natural? n) #(inexact-real? lo) #(inexact-real? hi))
     (assert (< lo hi))
     (let* ((d (- hi lo))
	    (res (make-f64vector (inc n)))
	    (n+1 (inc n))
	    (1/n (exact->inexact (/ 1 n))))
       (declare (not safe))
       (for..< (i 0 n+1)
	       (f64vector-set! res i
			       (fl+ lo (fl* d (exact->inexact i) 1/n))))
       res))

(TEST
 > (gen-buckets 10 0. 1.)
 ;; #f64(0. .1 .2 .3 .4 .5 .6 .7 .8 .9 1.)
 #f64(0. .1 .2 .30000000000000004 .4 .5 .6000000000000001 .7000000000000001 .8 .9 1.))


(def. (u32vector.chop-both v)
  (let ((len (u32vector-length v)))
    (subu32vector v 1 (dec len))))

(TEST
 > (.chop-both (u32vector 0 7 0))
 #u32(7))

(TEST
 > (def nums (random-f64vector 1000000))
 > (def buckets (gen-buckets 1000 0. 1.))
 > (equal? (.chop-both (bins nums buckets))
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
#pragma omp parallel for                                        \\
    private(i)                                                  \\
    schedule(dynamic,100)
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
    __atomic_add_fetch(&(res[bi]), 1, __ATOMIC_RELAXED);
}
"
		    nvals
		    vals
		    nbuckets
		    buckets
		    res))
	 res)))

(TEST
 > (def nums (binnums 1000000)) ;; uneven distribution
 > (equal? (.chop-both (bins* nums buckets))
	   (bin nums 1000))
 #t)


(def (timings)
     (def nums (binnums 1000000))
     (for-each (lambda (n)
		 (def buckets (gen-buckets n 0. 1.))
		 (println "--------------------------")
		 (println n)
		 (assert (equal? (.chop-both (bins* nums buckets))
				 (bin* nums n))))
	       (list 1000
		     10000
		     100000
		     1000000
		     10000000)))

