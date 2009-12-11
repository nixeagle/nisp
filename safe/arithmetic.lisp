;;; These package and function definitions are *alpha* quality.

(in-package :nisp-safe-system)

(defmacro define-export-system (name from &rest symbol-list)
  `(defpackage ,name
     (:use)
     (:import-from ,from ,@(values symbol-list))
     (:export ,@(values symbol-list))))


;;; These should be fairly safe as there are no side effects,
;;; at least according to the standard.

(define-export-system #:safe-arithmetic :cl
  #:+
  #:-
  #:*
  #:/
  #:=
  ;; basic incrementers
  #:1+
  #:1-
  ;; common denometer stuff
  #:gcd
  #:lcm
  #:abs
  #:conjugate
  #:cis
  #:sqrt
  #:isqrt
  #:exp
  #:expt
  #:log
  #:signum
  #:phase
  pi)

(define-export-system #:safe-arithmetic-trig :cl
  #:sin
  #:cos
  #:tan
  #:asin
  #:acos
  #:atan
  #:sinh
  #:cosh
  #:tanh
  #:asinh
  #:acosh
  #:atanh)

(define-export-system #:safe-arithmetic-comparision :cl
  #:/=
  #:<
  #:<=
  #:=
  #:>
  #:>=
  #:oddp
  #:evenp
  #:plusp
  #:max
  #:zerop
  #:min
  #:minusp)

(define-export-system #:safe-arithmetic-type-manipulation :cl
  #:ceiling
  #:float-radix
  #:rational
  #:complex
  #:float-sign
  #:rationalize  
  #:decode-float
  #:floor
  #:realpart     
  #:denominator
  #:fround
  #:rem          
  #:fceiling
  #:ftruncate
  #:round        
  #:ffloor
  #:imagpart
  #:scale-float  
  #:float
  #:integer-decode-float
  #:truncate     
  #:float-digits
  #:mod                                
  #:float-precision
  #:numerator)

(define-export-system #:safe-arithmetic-boole :cl
  #:ash
  #:boole
  boole-1
  boole-2
  boole-and
  boole-andc1
  boole-andc2
  boole-c1
  boole-c2
  boole-clr
  boole-eqv
  boole-ior
  boole-nand
  boole-nor
  boole-orc1
  boole-orc2
  boole-set
  boole-xor
  #:integer-length
  #:logand
  #:logandc1
  #:logandc2
  #:logbitp
  #:logcount
  #:logeqv
  #:logior
  #:lognand
  #:lognor
  #:lognot
  #:logorc1
  #:logorc2
  #:logtest
  #:logxor)

(define-export-system #:safe-arithmetic-byte :cl
  #:byte
  #:deposit-field
  #:ldb-test    
  #:byte-position
  #:dpb
  #:mask-field  
  #:byte-size
  #:ldb)

(define-export-system #:safe-arithmetic-implentation-constants :cl
  double-float-epsilon most-negative-fixnum           
    double-float-negative-epsilon  most-negative-long-float       
    least-negative-double-float    most-negative-short-float      
    least-negative-long-float      most-negative-single-float     
    least-negative-short-float     most-positive-double-float     
    least-negative-single-float    most-positive-fixnum           
    least-positive-double-float    most-positive-long-float       
    least-positive-long-float      most-positive-short-float      
    least-positive-short-float     most-positive-single-float     
    least-positive-single-float    short-float-epsilon            
    long-float-epsilon             short-float-negative-epsilon   
    long-float-negative-epsilon    single-float-epsilon           
    most-negative-double-float     single-float-negative-epsilon)

;;; These do modifications to variables, eg side effects.
(sb-int:sane-package)
(defpackage #:safe-arithmetic!
  (:use :cl)
  (:export
   ;; basic incrementers
   #:decf
   #:incf))
