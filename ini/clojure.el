(setq inferior-lisp-program
                                        ; Path to java implementation
      (let* ((java-path "java")
                                        ; Extra command-line options
                                        ; to java.
             (java-options "-Xms100M -Xmx600M")
                                        ; Base directory to Clojure.
                                        ; Change this accordingly.
             (clojure-path "/usr/local/clojure/")
                                        ; The character between
                                        ; elements of your classpath.
             (class-path-delimiter ":")
             (class-path (mapconcat (lambda (s) s)
                                        ; Add other paths to this list
                                        ; if you want to have other
                                        ; things in your classpath.
                                    (list (concat clojure-path "clojure.jar")
       (concat clojure-path "clojure-contrib.jar"))
                                    class-path-delimiter)))
        (concat java-path
                " " java-options
                " -cp " class-path
                " clojure.lang.Repl")))
 
;; Require clojure-mode to load and associate it to all .clj files.

(require 'clojure-mode)
