;;; Hackity hack to load configuration file _last_.

(in-package :nisp.i)

(load (merge-pathnames "config.lisp" +root-directory+)
      :if-does-not-exist nil)