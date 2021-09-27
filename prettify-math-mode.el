;;; mathjax-mode.el --- prettify math formula  -*- lexical-binding: t -*-
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
                         (lambda (exp)
                           (jsonrpc-request conn :asciimath2svg exp))))

(defvar default-delimiter "$$")

(defun delimiter-to-regexp (delimiter)
  (let* ((dlmt-beginning (cond ((consp delimiter) (car delimiter))
                               ((atom delimiter) delimiter)))
         (dlmt-end (cond ((consp delimiter) (cdr delimiter))
                         ((atom delimiter) delimiter))))
    (concat (regexp-quote dlmt-beginning)
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

;; point is dangerous to use, as don't know which code is using it
;; nil returned
(defun dummy-face-fn ()
  (let* ((start (match-beginning 0))
         (end (match-end 0))
         (mathexp (match-string 1)))
    (with-silent-modifications
      (if (get-text-property start 'focus-on)
          (remove-text-properties start end '(display))
        (setq cursor-sensor-inhibit nil)
        (cursor-sensor-mode 1)
        (setq pre-redisplay-functions (delq 'cursor-sensor--detect pre-redisplay-functions))
        (add-text-properties start end `(display ((image . (:type svg
                                                                  :data ,(mathexp-to-svg mathexp)
                                                                  :scale 1.8))
                                                  (raise 0.4))
                                                 cursor-sensor-functions (update-focus-on)
                                                 rear-nonsticky (cursor-sensor-functions))))))
  nil)


;; syntax class is mostly exclusive
;; but $ may be used both as word & delimiter
;; so only keyword is suitble here
;; display.image is dyna computed for each content
;;
;; HACK!!! following prettify symbols
;; 2 effects:
;; (dummy-face-fn) result as value of face
;; (dummy-face-fn) side effects
;; a bug here, as should NOT change it's face value
(defun register-in-font-lock ()
  (font-lock-add-keywords nil
                          `((,(delimiter-to-regexp default-delimiter) 0 (dummy-face-fn))))
  (--> '(display cursor-sensor-functions rear-nonsticky)
       (append it font-lock-extra-managed-props)
       (setq font-lock-extra-managed-props it)))

(register-in-font-lock)

(provide 'mathjax-mode)
;;; mathjax-mode.el ends here
