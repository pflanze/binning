(include "set-compiler.scm")
(include "set-config.scm")

(begin
  (c/load "lib/cj-source") ;; now loaded from define-macro-star.scm--ehr.needed ther
  (c/load "lib/define-macro-star")
  (c/load "lib/dummy-module")
  (c/load "lib/cj-phasing")

  (c/load "lib/srfi-1")
  (c/load "lib/cj-warn")
  (c/load "lib/cj-env-1")
  (c/load "lib/simple-match-1")
  (c/load "lib/test")
  (c/load "lib/cj-inline")
  (c/load "lib/cj-env")

  (c/load "lib/simple-match") ;; tests only

  (c/load "lib/define-nested")
  (c/load "lib/list-util-1")
  (c/load "lib/string-util-1")
  (c/load "lib/list-util") ;; for improper-fold for srfi-11
  (c/load "lib/string-util")
  (c/load "lib/srfi-11")
  (c/load "lib/slib-sort")
  (c/load "lib/cj-symbol") ;; for tests
  (c/load "lib/list-util-2")
  (c/load "lib/string-quote")
  (c/load "lib/cj-functional") ;; requires apply-values from srfi-11.scm
  (c/load "lib/cj-curry")

  (c/load "lib/lazy")

  (c/load "lib/cut")

  (c/load "lib/cj-struct")

  (c/load "lib/define-strict-and-lazy") ;; XX move to some real lib?

  (c/load "lib/cj-cmp")
  (c/load "lib/cj-alist")
  (c/load "lib/check-equal")

  (c/load "lib/define-module")

  (c/load "lib/cj-source-2")
  (c/load "lib/cj-match")
  (c/load "lib/cj-source-util-2")

  (c/load "lib/cj-expansion")
  (c/load "lib/cj-typed")
  (c/load "lib/stream")
  (c/load "lib/weak-srfi-1")
  (c/load "lib/test-lib") ;; should only be needed at run time but oo needs it here hm

  (c/load "lib/partial-apply")
  (c/load "lib/cj-functional-2")

  (c/load "lib/cj-port")

  (c/load "lib/cj-shortcuts")
  (c/load "lib/cj-env-2")

  (c/load "lib/cj-math")
  (c/load "lib/predicates")

  ;; only needed at runtime (when running the tests)
  (c/load "lib/lazy-debug")

  (c/load "lib/string-util-2")

  (c/load "lib/cj-ffi")

  (c/load "lib/vector-util")

  (c/load "lib/char-util")
  (c/load "lib/cj-syntax")

  (c/load "lib/cj-source-wraps")
  (c/load "lib/dot-oo")

  (c/load "lib/string-util-3")

  (c/load "lib/cj-source-quasiquote")
  (c/load "lib/cj-inline-2")
  (c/load "lib/constants")

  (c/load "lib/oo-util")
  (c/load "lib/easy")
  (c/load "lib/code-map")
  (c/load "lib/oo-vector-lib")

  (c/load "lib/u8-parse")
  (c/load "lib/cj-io-util")
  (i/load "lib/cj-path"))

(c/load "lib/local-test")

(c/load "lib/enum")

(c/load "binning"
	cc-options: "-pthread -fopenmp"
	ld-options: "-lrt -lgomp")

(c/load "morebinning"
	cc-options: "-pthread -fopenmp"
	ld-options: "-lrt -lgomp")

