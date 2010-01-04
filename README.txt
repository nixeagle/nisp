                  nisp -- experimentation and learning
                  ====================================



As of v0.0.13 most if not all of the dependencies that are not asdf
installable are direct submodules of nisp itself. When cloning a fresh
copy please do the following:

 cd nisp/
 git submodule update --init

This will pull in all the submodules.

Then in your lisp configuration file add a line similar to 

 (pushnew "/path/to/nisp/asdf/" asdf:*central-registry* :test #'equal)

Then start your lisp implentation, [1] and type:
 (asdf:load-system :nisp)

Table of Contents
=================
1 Problems 


1 Problems 
~~~~~~~~~~~
  Please report either to the github tracker or to me directly on
  [irc://irc.eighthbit.net/programming].



[1] Currently known to work only on sbcl 1.0.31 and 1.0.34

