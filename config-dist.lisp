;;; Please note that this config layout is very temporary.  Expect
;;; breaking changes, though notice will be given in some form.

(in-package :nispbot-config)
(5am:def-suite config-suite
    :in nisp::all-tests)
(in-suite config-suite)

(defparameter *nickname* "somenick"
  "The nick name of the bot. Be warned that this layout
is likely to be very temporary as we plan on moving into a
CLOS style.")

(defparameter *eighthbit* "platinum.eighthbit.net"
  "The network to connect to. Currently we can connect only
to one network at a time. This is a bug and will be fixed.")

(defparameter *channel* "#lisp, #bots"
  "The channel to join after connecting. The format of this
is subject to change pretty soon.")

(test *channel*-is-a-string
  "Make sure that we don't change the type without letting users know."
  (is (stringp nispbot-config::*channel*)))