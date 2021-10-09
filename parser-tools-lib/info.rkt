#lang info

(define collection 'multi)
(define deps '("scheme-lib"
               "base"
               "compatibility-lib"))
(define build-deps '("rackunit-lib"))

(define pkg-desc "implementation (no documentation) part of \"parser-tools\"")

(define pkg-authors '(mflatt))

(define license
  '(Apache-2.0 OR MIT))
