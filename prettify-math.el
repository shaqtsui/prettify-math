;;; prettify-math.el --- Prettify math formula -*- lexical-binding: t -*-

;; Author: Fucheng Xu <xfcjscn@163.com>
;; Maintainer: Fucheng Xu <xfcjscn@163.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (dash "2.19.0") (jsonrpc "1.0.9"))
;; Homepage: https://gitee.com/xfcjscn/prettify-math
;; Keywords: math asciimath tex latex prettify 2-d mathjax


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Prettify math is a EMACS minor mode to prettify math formulas.
;;
;; It's base on mathjax, refer mathjax for math formula related
;; stuffes.  Default math formula delimiters: $$ -> tex math, ` ->
;; asciimath.
;;
;; Prerequire
;;   nodejs - used to run mathjax, simple installation refer:
;;     https://nodejs.dev/download/package-manager
;;
;; Installation
;;   install `prettify-math` from melpa
;;
;; Usage
;;   enable prettify-math-mode in your buffer, or globally via
;;   global-prettify-math-mode.
;;
;; Customization
;;   You can customize delimiter before this module loaded.
;;   Code example in init.el:
;;   (setq prettify-math-delimiters-alist '(("$$" . tex)
;;     ("$" . tex)
;;     ("``" . asciimath)))
;;   (require 'prettify-math)

;;; Code:

(require 'jsonrpc)
(require 'dash)

(defconst prettify-math--pkg-base (if load-file-name (file-name-directory load-file-name) "./"))
(setq default-directory (expand-file-name prettify-math--pkg-base))

(defun prettify-math--init-mathjax ()
  "Install mathjax dependencies."
  (unless (file-exists-p (expand-file-name "package-lock.json" prettify-math--pkg-base))
    (call-process "npm" nil "*init-mathjax*" nil "install")))

(declare-function prettify-math--mathexp-to-svg "prettify-math" t t)
(fset 'prettify-math--mathexp-to-svg (let* ((_ (prettify-math--init-mathjax))
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
(defvar prettify-math-delimiters-alist
  '(("$$" . tex)
    ("`" . asciimath)))

(defun prettify-math--delimiter-to-regexp (delimiter)
  "Regexp for expression inside DELIMITER."
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


(defun prettify-math--update-focus-on (_ old-pos action)
  "Update text property focus-on.
Base on OLD-POS to calculate texts when ACTION is entered, otherwise on point."
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



(defun prettify-math--facespec-fn ()
  "Property face only specified when its value non-nil.
display.image is dyna computed for each content.
Unfontify before fontify?"
  (let* ((start (match-beginning 0))
         (dlmt (match-string 1))
         (mathexp (match-string 2)))
    (if (get-text-property start 'focus-on)
        `(face nil cursor-sensor-functions (prettify-math--update-focus-on)
               rear-nonsticky (cursor-sensor-functions))
      `(face nil display ((image . (:type svg
                                          :data ,(prettify-math--mathexp-to-svg mathexp (assoc-default dlmt prettify-math-delimiters-alist))
                                          :scale 1.8))
                          (raise 0.4))
             cursor-sensor-functions (prettify-math--update-focus-on)
             rear-nonsticky (cursor-sensor-functions)))))

(defvar prettify-math--keywords
  (--map (list (prettify-math--delimiter-to-regexp (car it))
               0
               '(prettify-math--facespec-fn))
         prettify-math-delimiters-alist))

(defvar prettify-math--extra-properties
  '(display cursor-sensor-functions rear-nonsticky))


(defun prettify-math--register-in-font-lock ()
  "Only keyword is suitble here.
As syntax class is mostly exclusive."
  (cursor-sensor-mode 1)
  (setq pre-redisplay-functions (delq 'cursor-sensor--detect pre-redisplay-functions))
  (font-lock-add-keywords nil prettify-math--keywords)
  (--> prettify-math--extra-properties
       (append it font-lock-extra-managed-props)
       (setq font-lock-extra-managed-props it)))

(defun prettify-math--unregister-in-font-lock ()
  "Remove keywords."
  (cursor-sensor-mode -1)
  (font-lock-remove-keywords nil prettify-math--keywords)
  (setq font-lock-extra-managed-props (--remove (memq it prettify-math--extra-properties)
                                                font-lock-extra-managed-props)))

;;;###autoload
(define-minor-mode prettify-math-mode
  "prettify math mode base on font lock"
  :lighter " pmath"
  (if prettify-math-mode
      (progn
        (prettify-math--register-in-font-lock)
        (font-lock-flush))
    (prettify-math--unregister-in-font-lock)
    (with-silent-modifications
      (remove-list-of-text-properties (point-min) (point-max)
                                      (cons 'focus-on prettify-math--extra-properties)))))

;;;###autoload
(define-globalized-minor-mode global-prettify-math-mode
  prettify-math-mode
  prettify-math-mode)

(provide 'prettify-math)

;;; prettify-math.el ends here
