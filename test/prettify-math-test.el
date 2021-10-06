;;; prettify-math-test.el --- Tests for prettify-math
(ert-deftest prettify-math-load-test ()
  "file can be loaded?"
  (should (require 'prettify-math)))

(ert-deftest prettify-math-facespec-test ()
  "facespec test"
  (require 'prettify-math)
  (with-temp-buffer
    (insert " $$ a $$ ")
    (goto-char (point-min))
    (re-search-forward (prettify-math--delimiter-to-regexp "$$") (point-max) t)
    (should (prettify-math--facespec-fn))))

(ert-deftest prettify-math-mode-test ()
  "mode test"
  (require 'prettify-math)
  (with-temp-buffer
    (insert " $$ a $$ ")
    (goto-char (point-min))
    (should (prettify-math-mode))
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
