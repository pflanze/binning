;; see binning.scm first for the simpler algorithms

(declare (standard-bindings) (extended-bindings) (block))

(def (0..1->uint x bits)
     (integer (* x (arithmetic-shift 1 bits))))

(def (0..1->uint32 x)
     (0..1->uint x 32))

(TEST
 > (0..1->uint32 0.)
 0
 > (0..1->uint32 1.)
 4294967296
 > (0..1->uint32 .99999999999)
 4294967295)

(c-declare "
static unsigned long long
double2uint32 (double x) {
    if (x < 0) {
        return 0; // XX
    } else {
        unsigned long long int o= x * (1ll<<32);
        return o >= (1ll<<32) ? (1ll<<32)-1 /*XX*/ : o;
    }
}
")

(def (do-0..1->uint32 xs out)
     (let ((len (f64vector-length xs)))
       (assert (= len (u32vector-length out)))
       (##c-code "
double *xs= ___BODY(___ARG1);
unsigned int *out= ___BODY(___ARG2);
int n=  ___INT(___ARG3);

int i;
for (i=0; i<n; i++) {
    out[i]= double2uint32(xs[i]);
}
"
		 xs
		 out
		 len)))


(def (0..1->uint32* x)
     (let ((out (u32vector 0)))
       (do-0..1->uint32 (f64vector x) out)
       (u32vector-ref out 0)))

(TEST
 > (0..1->uint32* 0.)
 0
 > (0..1->uint32* 1.)
 4294967295
 > (0..1->uint32* .99999999999)
 4294967295)


(TEST
 > (def (t n)
	(.for-each (random-f64vector n)
		   (lambda (x)
		     (assert (= (0..1->uint32 x)
				(0..1->uint32* x))))))
 ;; > (t 1000000)
 > (t 10000))

