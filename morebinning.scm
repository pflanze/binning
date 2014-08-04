(include "bin.scm")

;; Idea 2: have an even bigger level atop the buckets array that
;; doesn't fit into the CPU cache, still using parts of the key as
;; address.  Since only one lookup is done, it will still cut down on
;; memory accesses.

;; e.g. use 24 bits of the 52 bits of the coefficient of the double
;; value. Will need about 32 MB for the first level map (mapping
;; address to 16 bits of pointer value, using an in-cache map to find
;; out what the remaining bits of the pointer value are?; or, even
;; just use all the necessary bits directly, with 10000000 buckets,
;; that's 24 bits, hence will need 24* 2**24 bits = 48 MB), then if
;; the bucket widths aren't very uneven, one single lookup then
;; reduces the search by close to 24 bits which is already close to
;; all of it, hence an improvement from about 22 memory accesses (the
;; last about 2 steps are using cache lines that have already been
;; read) to 2-3, almost 10 fold. Even if say 8 bits of binary search
;; remain because the widths vary by a factor of say 256, that's a
;; reduction from about 22 accesses to about 1+6 accesses, about 3.1
;; fold.

(def (getbits32 bit-index numbits)
     (lambda (i)
       (bitwise-and (arithmetic-shift i (- bit-index (- 32 numbits)))
		    (- (arithmetic-shift 1 numbits) 1))))

(c-declare "
static int
getbits32 (int bit_index, int numbits, unsigned int i) {
    return ((i >> ((32 - numbits) - bit_index) &
            ((1 << numbits)-1)));
}
")
(def (getbits32* #(fixnum? bit-index) #(fixnum? numbits))
     (lambda (i)
       (let ((is (u32vector i))
	     (res (s32vector 0)))
	 (##c-code "
unsigned int *is= ___BODY(___ARG1);
int *res= ___BODY(___ARG2);
int bit_index= ___INT(___ARG3);
int numbits= ___INT(___ARG4);

res[0]= getbits32(bit_index,numbits,is[0]);
"
		   is res bit-index numbits)
	 (.ref res 0))))
;; yes that would have been easier with c-lambda

(TEST
 > (def (t-getbits getbits32)
	(local-TEST
	 > (.string ((getbits32 0 4) (arithmetic-shift 1 31)) 2)
	 "1000"
	 > (.string ((getbits32 0 4) (arithmetic-shift 1 30)) 2)
	 "100"
	 > (.string ((getbits32 1 4) (arithmetic-shift 1 30)) 2)
	 "1000"
	 > (.string ((getbits32 1 5) (arithmetic-shift 1 30)) 2)
	 "10000"
	 > (.string ((getbits32 2 5) (arithmetic-shift 1 30)) 2)
	 "0"
	 > (.string ((getbits32 32 5) 1) 2)
	 "0"
	 ;; > (.string ((getbits32 31 5) 1) 2)
	 ;; "10000" ;; hm interesting; whatever--ah, C version gives 0
	 ))
 > (%test (t-getbits getbits32))
 > (%test (t-getbits getbits32*)))



;; level1 is a vector using level1-bits as index -> index-into-buckets
;; (XX more precisely, a value that can be passed to bins:search*,
;; which is 1 higher)

(compile-time
 (def buckets->level1-safe? #f))

(def (buckets->level1 buckets level1-bits)
     (IF (not buckets->level1-safe?) (declare (not safe)))
     ;; XXX assumes that min and max of buckets are 0. and 1.
     (let* ((siz (arithmetic-shift 1 level1-bits))
	    (l1 (make-u32vector (inc siz)))
	    (level1-max (arithmetic-shift 1 level1-bits))
	    (divider (exact->inexact (/ level1-max)))
	    (nbuckets (.length buckets))
	    (level1bits (getbits32 0 level1-bits)))
       (for..< (i 0 siz)
	       (let* ((idouble (* i divider)))
		 (IF buckets->level1-safe?
		     (assert (= i (level1bits (0..1->uint32 idouble)))))
		 ;; ^ XX is there a better way to solve it? walk
		 ;; doubles instead of integers and fill in holes (but
		 ;; how, correctly?)
		 (u32vector-set! l1 i
				 (bins:search idouble buckets nbuckets))))
       (u32vector-set! l1 siz nbuckets)
       l1))

(TEST
 > (def bs (gen-buckets 5 0. 1.))
 ;; > bs
 ;; #f64(0. .2 .4 .6000000000000001 .8 1.)
 > (buckets->level1 bs 0)
 #u32(1 6)
 > (buckets->level1 bs 1)
 #u32(1 3 6)
 > (buckets->level1 bs 2)
 #u32(1 2 3 4 6)
 > (buckets->level1 bs 3)
 #u32(1 1 2 2 3 4 4 5 6)
 > (buckets->level1 bs 4)
 #u32(1 1 1 1 2 2 2 3 3 3 4 4 4 5 5 5 6)
 )


(def (morebins vals buckets level1-bits)
     (let* ((level1 (buckets->level1 buckets level1-bits))
	    (nvals (.length vals))
	    (vals-u32 (make-u32vector nvals))
	    (level1bits (getbits32 0 level1-bits))
	    (nbuckets (.length buckets))
	    (res (make-u32vector (inc nbuckets))))
       (do-0..1->uint32 vals vals-u32)
       (for..< (i 0 nvals)
	       (let* ((x (f64vector-ref vals i)))
		 (u32vector-inc!
		  res
		  (if (< x 0.)
		      0
		      (let* ((xint (u32vector-ref vals-u32 i))
			     (i0 (level1bits xint))
			     (j1 (u32vector-ref level1 i0))
			     (j2 (u32vector-ref level1 (inc i0))))
			((bins:search* x buckets nbuckets) j1 j2))))))
       res))

(defmacro (make-morebins* parallel?)
  (quasiquote-source
   (lambda (vals buckets level1-bits)
     (let* ((level1 (buckets->level1 buckets level1-bits))
	    (nvals (.length vals))
	    (nbuckets (.length buckets))
	    (res (make-u32vector (inc nbuckets))))
       (time (##c-code ,(string-append "
int nvals= ___INT(___ARG1);
double *vals = ___BODY(___ARG2);
int nbuckets = ___INT(___ARG3);
double* buckets= ___BODY(___ARG4);
unsigned int *level1 = ___BODY(___ARG5);
int level1bits= ___INT(___ARG6);
unsigned int *res = ___BODY(___ARG7);

int i;" (if (eval parallel?) "
#pragma omp parallel for                                        \\
    private(i)                                                  \\
    schedule(dynamic,100)"
	    "") "
for (i=0; i<nvals; i++) {
    double x= vals[i];
    int bi;
    if (x < 0) {
        bi= 0;
    } else {
        unsigned int xint= double2uint32(x);
        int i0= getbits32(0, level1bits, xint);
        int lo= level1[i0];
        int hi= level1[i0+1];
        while (lo < hi) {
            int mid= (lo+hi) / 2;
            if (x < buckets[mid]) {
                hi= mid;
            } else {
                lo= mid+1;
            }
        }
        bi= lo;
    }
    __atomic_add_fetch(&(res[bi]), 1, __ATOMIC_RELAXED);
}
")
	     nvals
	     vals
	     nbuckets
	     buckets
	     level1
	     level1-bits
	     res))
       res))))

(def morebins* (make-morebins* #f))
(def parallel-morebins* (make-morebins* #t))

(TEST
 > (def (t morebins)
	(list-uniq equal?
		   (map (C morebins
			   (f64vector -0.1
				      0.
				      0.5 0.6
				      .6000000000000001
				      0.999
				      1. 100.)
			   (gen-buckets 10 0. 1.)
			   _)
			'(0 1 2 3 4 5 6 7 8 9 10 11 12))))
 > (t morebins)
 (#u32(1 1 0 0 0 0 2 1 0 0 1 2))
 > (t morebins*)
 (#u32(1 1 0 0 0 0 2 1 0 0 1 2))
 > ((lambda (n b)
      (let ((nums (gen-binnums 1000000)))
	(equal? (bin* nums n)
		(.chop-both-ends (parallel-morebins* nums (gen-buckets n 0. 1.) b)))))
    10000 16)
 #t)
