;;; prettify-math-test.el --- Tests for prettify-math

;;; Commentary:
;; 

;;; Code:

;; checkdoc:
;; cask emacs --batch --file prettify-math.el --funcall checkdoc

;; compile check:
;; cask emacs --batch --funcall batch-byte-compile prettify-math.el
;;
;; ert runner:
;; cask exec ert-runner
;; 
;; manual test:
;; cask build
;; cask install
;; cask emacs -Q

(ert-deftest prettify-math-load-test ()
  "file can be loaded?"
  (should (require 'prettify-math)))

(ert-deftest prettify-math-replace-test ()
  "replace test"
  (require 'prettify-math)
  (should (equal (prettify-math--update-attr "<test a=\"b\" cd=\"shaq\" />" "test" "cd" (lambda (x) "ddd"))
                 "<test a=\"b\" cd=\"ddd\" />")))

(ert-deftest prettify-math-scale-test ()
  "scale svg test"
  (require 'prettify-math)
  (should (equal (prettify-math--scale-svg "<svg height=\"2em\" width=\"1px\" a=\"b\" cd=\"shaq\" />" 2)
                 "<svg height=\"4em\" width=\"2px\" a=\"b\" cd=\"shaq\" />")))

(ert-deftest prettify-math-facespec-test ()
  "facespec test"
  (require 'prettify-math)
  (with-temp-buffer
    (insert " $$ a $$ ")
    (goto-char (point-min))
    (re-search-forward (prettify-math--delimiter-to-regexp "$$") (point-max) t)
    (should (prettify-math--facespec-fn))))

(ert-deftest prettify-math-mathjax-test ()
  "install mathajx"
  (require 'prettify-math)
  (unwind-protect
      (progn
        (prettify-math--ensure-mathjax)
        (should (file-exists-p (expand-file-name "package-lock.json" prettify-math--mathjax-workspace))))
    (message "pls remove files")
    ))

(ert-deftest prettify-math-mjserver-test ()
  "mj server will exist after test, as sub process terminate"
  (require 'prettify-math)
  (unwind-protect
      (progn
        (prettify-math--ensure-mjserver)
        (should prettify-math--mjserver)
        (should (process-live-p prettify-math--mjserver)))
    (setq prettify-math--mjserver nil)))

(ert-deftest prettify-math-conn-test ()
  "mj server will exist after should, as sub process terminate"
  (require 'prettify-math)
  (unwind-protect
      (progn
        (prettify-math--ensure-conn)
        (should prettify-math--conn))
    (setq prettify-math--conn nil)))

(ert-deftest prettify-math-mathexp-test ()
  (require 'prettify-math)
  (unwind-protect
      (progn
        (setq res (prettify-math--mathexp-to-svg "f"))
        (should res))))

(ert-deftest prettify-math-mode-test ()
  "mode test"
  (require 'prettify-math)
  (with-temp-buffer
    (insert " $$ a $$ ")
    (goto-char (point-min))
    ;; cannot answer question in ert-runner
    ;;(should (prettify-math-mode))
    (should-not (prettify-math-mode -1))))

(ert-deftest prettify-math-delimiter-test ()
  (require 'prettify-math)
  (setq-local prettify-math-delimiters-alist '(("$" tex)
                                  (("\\(" . "\\)") tex block)
                                  ("`" asciimath)
                                  ("``" asciimath block)))
  (should (equal (prettify-math-type-by-delimiter-beg "$") 'tex))
  (should-not (equal (prettify-math-type-by-delimiter-beg "$$") 'tex))
  (should (equal (prettify-math-type-by-delimiter-beg "\\(") 'tex))
  (should (equal (prettify-math-type-by-delimiter-beg "`") 'asciimath))
  (should (equal (prettify-math-type-by-delimiter-beg "``") 'asciimath))
  (should (prettify-math-block-delimiters))
  (should (prettify-math-contains-block-delimiters-p))
  (setq-local prettify-math-delimiters-alist '(("$" tex)
                                  (("\\(" . "\\)") tex)
                                  ("`" asciimath)
                                  ("``" asciimath)))
  (should-not (prettify-math-block-delimiters))
  (should-not (prettify-math-contains-block-delimiters-p))
  )



;;; prettify-math-test.el ends here
