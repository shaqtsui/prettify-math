;;; prettify-math-mode.el --- prettify math formula  -*- lexical-binding: t -*-

;; Copyright: GPL

;; Author: Fucheng Xu <xfcjscn@163.com>

;;; Commentary:
;; this long desc is not used

;;; Code:
(require 'jsonrpc)
(require 'dash)

(defconst prettify-math-mode--pkg-base (if load-file-name (file-name-directory load-file-name) "./"))
(setq default-directory (expand-file-name prettify-math-mode--pkg-base))

(defun prettify-math-mode--init-mathjax ()
  (unless (file-exists-p (expand-file-name "package-lock.json" prettify-math-mode--pkg-base))
    (call-process "npm" nil "*init-mathjax*" nil "install")))

(declare-function prettify-math-mode--mathexp-to-svg "prettify-math-mode" t t)
(fset 'prettify-math-mode--mathexp-to-svg (let* ((_ (prettify-math-mode--init-mathjax))
                             (mjserver (make-process :name "mjserver"
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

;; can't use customize, as init depdent on it
(defvar prettify-math-mode-delimiters-alist
  '(("$$" . tex)
    ("`" . asciimath)))

(defun prettify-math-mode--delimiter-to-regexp (delimiter)
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


(defun prettify-math-mode--update-focus-on (window old-pos action)
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
(defun prettify-math-mode--facespec-fn ()
  (let* ((start (match-beginning 0))
         (dlmt (match-string 1))
         (mathexp (match-string 2)))
    (if (get-text-property start 'focus-on)
        `(face nil cursor-sensor-functions (update-focus-on)
               rear-nonsticky (cursor-sensor-functions))
      `(face nil display ((image . (:type svg
                                          :data ,(prettify-math-mode--mathexp-to-svg mathexp (assoc-default dlmt prettify-math-mode-delimiters-alist))
                                          :scale 1.8))
                          (raise 0.4))
             cursor-sensor-functions (update-focus-on)
             rear-nonsticky (cursor-sensor-functions)))))

(defvar prettify-math-mode--keywords
  (--map (list (prettify-math-mode--delimiter-to-regexp (car it))
               0
               '(facespec-fn))
         prettify-math-mode-delimiters-alist))

(defvar prettify-math-mode--extra-properties
  '(display cursor-sensor-functions rear-nonsticky))

;; syntax class is mostly exclusive
;; but $ may be used both as word & delimiter
;; so only keyword is suitble here
;; display.image is dyna computed for each content
(defun prettify-math-mode--register-in-font-lock ()
  (cursor-sensor-mode 1)
  (setq pre-redisplay-functions (delq 'cursor-sensor--detect pre-redisplay-functions))
  (font-lock-add-keywords nil prettify-math-mode--keywords)
  (--> prettify-math-mode--extra-properties
       (append it font-lock-extra-managed-props)
       (setq font-lock-extra-managed-props it)))

(defun prettify-math-mode--unregister-in-font-lock ()
  (cursor-sensor-mode -1)
  (font-lock-remove-keywords nil prettify-math-mode--keywords)
  (setq font-lock-extra-managed-props (--remove (memq it prettify-math-mode--extra-properties)
                                                font-lock-extra-managed-props)))

;;;###autoload
(define-minor-mode prettify-math-mode
  "prettify math mode base on font lock"
  :lighter " pmath"
  (if prettify-math-mode
      (progn
        (prettify-math-mode--register-in-font-lock)
        (font-lock-flush))
    (prettify-math-mode--unregister-in-font-lock)
    (with-silent-modifications
      (remove-list-of-text-properties (point-min) (point-max) prettify-math-mode--extra-properties))))

;;;###autoload
(define-globalized-minor-mode global-prettify-math-mode
  prettify-math-mode
  prettify-math-mode)

(provide 'prettify-math-mode)
;;; prettify-math-mode.el ends here
