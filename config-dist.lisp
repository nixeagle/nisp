;;; Please note that this config layout is very temporary.  Expect
;;; breaking changes, though notice will be given in some form.
(defpackage #:nispbot-config
  (:use :common-lisp :lift
        )
  (:export *channel* *nickname* *eighthbit*))

(in-package :nispbot-config)

(deftestsuite root-suite (nisp::root-suite) ())

(defparameter *nickname* "somenick"
  "The nick name of the bot. Be warned that this layout
is likely to be very temporary as we plan on moving into a
CLOS style.")

(defvar *server* "platinum.eighthbit.net"
  "The network to connect to. Currently we can connect only
to one network at a time. This is a bug and will be fixed.")

(defvar *channels* '("#chan1"  "#chan2" "chan3")
  "Default list of channels to join on connection. When the bot goes
  object orianted this will follow, but no matter what it will remain a
  list in the simplest case.")

(defvar *password* nil
  "Default password for irc instances.")

(defvar *comchar* #\!
  "Bot will parse all messages strting with this char.")

(defvar *developer-host* ""
  "Set this to person running the bot, allows extra commands.")

(defvar *admin-hosts* ()
  "List of people who are allowed to run full eval with no restrictions.")

(defvar *bot-hosts* ()
  "List of hosts to ignore by default.")

(defvar *freenode-password* ""
  "Temporary for storing the password to freenode.")

(deftestsuite test-*channels* (root-suite)
  ()
  :test (is-list
         (:documentation
          "Please use *channels* which is currently a list of
strings. *channel is depreciated.")
         (ensure (listp *channels*))))