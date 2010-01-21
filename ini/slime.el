
(add-to-list 'load-path "~/.elisp/pkgs/slime")

;; swank-clojure
(add-to-list 'load-path "~/.elisp/pkgs/swank-clojure/src/emacs")

(setq swank-clojure-jar-path "~/local/clojure/clojure.jar"
      swank-clojure-extra-classpaths (list
				      "~/.elisp/pkgs/swank-clojure/src/main/clojure"
				      "~/local/clojure/clojure-contrib.jar"))

(require 'swank-clojure-autoload)

;; slime
(eval-after-load "slime"
  '(progn
     (require 'slime-fuzzy)
     (setq slime-complete-symbol*-fancy t)
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
     (slime-setup '(slime-repl))))

(require 'slime)
(slime-setup)
