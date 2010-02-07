(in-package :with-fbound)

(test over-fbounds
  (signals (error "No args, expect 2") (macroexpand-1 '(with-fbound)))
  
  #+ () (is (equal (list 'progn) (macroexpand-1 '(with-fbound (+))))
            "Expect expansion of (progn) when given no additional arguments.")
  
  (signals (error "Should error when given just a docstring")
    (macroexpand-1 '(with-fbound (+) "a docstring/reason.")))
  
  (signals (error "Should error when given function args, but no expected result.")
    (macroexpand-1 '(with-fbound (+) (1 2 3))))
  
  (is (equal '(progn (is (eos::find-predicate 6 (+ 1 2 3)) "Should add to 6"))
             (macroexpand-1 '(with-fbound (+)
                              "Should add to 6"
                              (1 2 3) 6)))
      "Simplest generated expansion possible, one reason, one arglist one return.")
  
  (is (equal '(progn (finishes (+ 1 2 3)))
             (macroexpand-1 '(with-fbound (+)
                              "Should add to 6"
                              (1 2 3) :finishes)))
      "Should expand to (finishes ...)")
  
  (is (equal '(progn (finishes (+ 1 2 3)))
             (macroexpand-1 '(with-fbound (+)
                              "Should add to 6"
                              (1 2 3) :should-finish)))
      "Should expand to (finishes ...)")
  
  (is (equal '(progn (signals (error "Should add to 6") (+ 1 2 3)))
             (macroexpand-1 '(with-fbound (+)
                              "Should add to 6"
                              (1 2 3) :should-signal error)))
      "Should expand to (signals (error <formatstring> 'error docstring) ...)")
  
  (is (equal '(progn (signals (error "Should add to 6") (+ 1 2 3)))
             (macroexpand-1 '(with-fbound (+) "Should add to 6" (1 2 3) :signals error)))
      "Should expand to (signals (error <formatstring> 'error docstring) ...)")
  
  (is (equal '(progn
               (signals (error"Should add to 6")
                 (+ 1 2 3))
               (finishes
                 (+ 4))
               (is (eos::find-predicate 4 (+ 4)) "Adds to 4"))
             (macroexpand-1 '(with-fbound (+)
                              "Should add to 6"
                              (1 2 3) :should-signal error
                              "Adds to 4"
                              (4) :finishes
                              (4) 4)))
      "Should expand exactly. This tests complex expansion handling with~
      3 different expansions intermixed.")
  
  (is (equal '(progn (is (eos::find-predicate 6 (+ 1 2 3)) "Should add to 6"))
             (macroexpand-1 '(with-fbound (+)
                              "Should add to 6"
                              (1 2 3) 6)))
      "Simple generated expansion, but with a specified predicate.")

  (with-fbound (+)
    "Should add to 6 and make sure `with-fbound' actually works."
    (1 2 3) :satisfies integerp
    (1 2 3) :satisfies (not stringp))
  
  (with-fbound (+)
    (1 2 3) :should-be integer
    (1 2 3) :expects (not string))
  
  (with-fbound (+)
    (1 2 3) 6
    (1) 1
    (1 2 3) (not 5))
  (let ((x 0))
    (with-fbound (incf)
      "Verify actual result is evaluated before expected result."
      (x) (1- (incf x))
      (x) (not (1+ (incf x))))))