;;; Please note that this config layout is very temporary.  Expect
;;; breaking changes, though notice will be given in some form.

(in-package :nispbot-config)
(5am:def-suite config-suite
    :in nisp::all-tests)
(in-suite config-suite)

(deftestsuite root-suite (nisp::root-suite) ())

(defparameter *nickname* "somenick"
  "The nick name of the bot. Be warned that this layout
is likely to be very temporary as we plan on moving into a
CLOS style.")

(defparameter *eighthbit* "platinum.eighthbit.net"
  "The network to connect to. Currently we can connect only
to one network at a time. This is a bug and will be fixed.")

(defparameter *channel* nil
  "DEPRECIATED: The channel to join after connecting. The format of this
is subject to change pretty soon.")

(defparameter *channels* '("#chan1"  "#chan2" "chan3")
  "Default list of channels to join on connection. When the bot goes
  object orianted this will follow, but no matter what it will remain a
  list in the simplest case.")

(deftestsuite test-*channel* (root-suite)
  ()
  :test (is-nil
         (ensure-same *channel* nil :test #'eq)))

(deftestsuite test-*channels* (root-suite)
  ()
  :test (is-list
         (:documentation
          "Please use *channels* which is currently a list of
strings. *channel is depreciated.")
         (ensure (listp *channels*))))

;; (test *channel*-is-nil
;;   "Please use *channels* which is currently a list of strings. *channel is depreciated."
;;   (is (eq nil *channel*)))

;; (test *channels*-is-a-list
;;   (is (listp *channels*)))