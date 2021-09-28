;;; prettify-math-mode.el --- prettify math formula  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'jsonrpc)
(require 'dash)
(fset 'mathexp-to-svg (let* ((mjserver (make-process :name "mjserver"
                                                     :buffer "mjserver"
                                                     :command '("node" "mathjax-jsonrpc.js")
                                                     :connection-type 'pipe
                                                     ;; :stderr "myjservererr"
                                                     ))
                             (conn (jsonrpc-process-connection :process mjserver)))
                        (set-process-query-on-exit-flag mjserver nil)
                        (lambda (exp &optional type)
                          (jsonrpc-request conn
                                           (-> type
                                               (or 'asciimath)
                                               symbol-name
                                               (concat "2svg")
                                               make-symbol)
                                           exp))))

(defvar default-delimiters-alist
  '(("$$" . tex)
    ("`" . asciimath)))

(defun delimiter-to-regexp (delimiter)
  (let* ((dlmt-beginning (cond ((consp delimiter) (car delimiter))
                               ((atom delimiter) delimiter)))
         (dlmt-end (cond ((consp delimiter) (cdr delimiter))
                         ((atom delimiter) delimiter))))
    (concat "\\("
            (regexp-quote dlmt-beginning)
            "\\)"
            "\\("
            ".+?"
            "\\)"
            (regexp-quote dlmt-end))))


(defun update-focus-on (window old-pos action)
  (with-silent-modifications
    (if (eq action 'entered)
        (let ((s (previous-single-property-change (1+ (point)) 'display nil (point-min)))
              (e (next-single-property-change (point) 'display nil (point-max))))
          (put-text-property s e 'focus-on t)
          (font-lock-flush s e))
      (let ((s (previous-single-property-change (1+ old-pos) 'display nil (point-min)))
            (e (next-single-property-change old-pos 'display nil (point-max))))
        (remove-text-properties s e '(focus-on))
        (font-lock-flush s e)))))


;; unfontify before fontify?
;; face only specified when its value non-nil
(defun facespec-fn ()
  (let* ((start (match-beginning 0))
         (dlmt (match-string 1))
         (mathexp (match-string 2)))
    (if (get-text-property start 'focus-on)
        `(face nil cursor-sensor-functions (update-focus-on)
               rear-nonsticky (cursor-sensor-functions))
      `(face nil display ((image . (:type svg
                                          :data ,(mathexp-to-svg mathexp (assoc-default dlmt default-delimiters-alist))
                                          :scale 1.8))
                          (raise 0.4))
             cursor-sensor-functions (update-focus-on)
             rear-nonsticky (cursor-sensor-functions)))))

(defvar keywords
  (--map (list (delimiter-to-regexp (car it))
               0
               '(facespec-fn))
         default-delimiters-alist))

(defvar extra-properties
  '(display cursor-sensor-functions rear-nonsticky))

;; syntax class is mostly exclusive
;; but $ may be used both as word & delimiter
;; so only keyword is suitble here
;; display.image is dyna computed for each content
(defun register-in-font-lock ()
  (cursor-sensor-mode 1)
  (setq pre-redisplay-functions (delq 'cursor-sensor--detect pre-redisplay-functions))
  (font-lock-add-keywords nil keywords)
  (--> extra-properties
       (append it font-lock-extra-managed-props)
       (setq font-lock-extra-managed-props it)))

(defun unregister-in-font-lock ()
  (cursor-sensor-mode -1)
  (font-lock-remove-keywords nil keywords)
  (setq font-lock-extra-managed-props (--remove (memq it extra-properties)
                                                font-lock-extra-managed-props)))


(define-minor-mode prettify-math-mode
  "prettify math mode base on font lock"
  :lighter " pmath"
  (if prettify-math-mode
      (progn
        (register-in-font-lock)
        (font-lock-flush))
    (unregister-in-font-lock)
    (with-silent-modifications
      (remove-list-of-text-properties (point-min) (point-max) extra-properties))))

(provide 'prettify-math-mode)
;;; prettify-math-mode.el ends here
