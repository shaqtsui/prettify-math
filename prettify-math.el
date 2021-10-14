;;; prettify-math.el --- Prettify math formula -*- lexical-binding: t -*-

;; Author: Shaq Xu <shaqxu@163.com>
;; Maintainer: Shaq Xu <shaqxu@163.com>
;; Version: 0.5
;; Package-Requires: ((emacs "27.1") (dash "2.19.0") (s "1.12.0") (jsonrpc "1.0.9"))
;; Homepage: https://gitee.com/shaqxu/prettify-math
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
;;   (setq prettify-math-delimiters-alist '(("$" tex)
;;    (("\\(" . "\\)") tex block)
;;    ("`" asciimath)
;;    ("``" asciimath block)))
;;   (require 'prettify-math)

;;; Code:

(require 'jsonrpc)
(require 'dash)
(require 's)
(require 'dom)
(require 'svg)


;;;;;;;;;;;;;;;;;;; mathjax ;;;;;;;;;;;;;;;;;
(defconst prettify-math--pkg-base (expand-file-name (if load-file-name (file-name-directory load-file-name) "./")))


(defun prettify-math--ensure-mathjax ()
  "Install mathjax dependencies."
  (unless (file-exists-p (expand-file-name "package-lock.json" prettify-math--pkg-base))
    (let ((default-directory prettify-math--pkg-base))
      (call-process "npm" nil "*init-mathjax*" nil "install" "mathjax-full" "vscode-jsonrpc"))))

(defvar prettify-math--mjserver nil
  "Mathjax server process.")

(defun prettify-math--ensure-mjserver ()
  "Init mjserver if required."
  (unless (and prettify-math--mjserver
               (process-live-p prettify-math--mjserver))
    (prettify-math--ensure-mathjax)
    (setq prettify-math--mjserver (let ((default-directory prettify-math--pkg-base))
                       (make-process :name "mjserver"
                                     :buffer "mjserver"
                                     :command '("node" "-r" "esm" "mathjax-jsonrpc.js")
                                     :connection-type 'pipe
                                     ;;:stderr "myjservererr"
                                     )))
    (set-process-query-on-exit-flag prettify-math--mjserver nil))
  prettify-math--mjserver)

(defvar prettify-math--conn nil
  "Jsonrpc conn obj.")

(defun prettify-math--ensure-conn ()
  "Init conn if requried."
  (unless prettify-math--conn
    (prettify-math--ensure-mjserver)
    (setq prettify-math--conn (jsonrpc-process-connection :process prettify-math--mjserver)))
  prettify-math--conn)


(defun prettify-math-reset-conn ()
  "If server down, reset it."
  (setq prettify-math--mjserver nil)
  (setq prettify-math--conn nil))

;; (prettify-math-reset-conn)

(defun prettify-math--mathexp-to-svg (exp &optional type)
  "Convert math EXP of TYPE(tex, asciimath) to svg."
  (prettify-math--ensure-conn)
  (jsonrpc-request prettify-math--conn
                   (-> type
                       (or 'asciimath)
                       symbol-name
                       (concat "2svg")
                       make-symbol)
                   exp))

(defun prettify-math--to-dom (s)
  "Convert S to dom, if required."
  (with-temp-buffer
    (insert s)
    (libxml-parse-xml-region (point-min) (point-max))))

(defun prettify-math--dom-update-attr (node attr fn)
  "FN update NODE ATTR."
  (dom-set-attribute node attr
                     (funcall fn (dom-attr node attr))))


(defun prettify-math--string-to-nu (s)
  "String S to number with unit."
  (let ((num (car (s-match "[0-9.]+" s)))
        (unit (car (s-match "[[:alpha:]]+" s))))
    (cons (string-to-number num)
          unit)))

(defun prettify-math--nu-to-string (nu)
  "Number with unit NU to string."
  (concat (number-to-string (car nu))
          (cdr nu)))

(defun prettify-math--scale-nu (nu factor)
  "Scale NU by factor FACTOR."
  (cons (* factor (car nu))
        (cdr nu)))

(defun prettify-math--scale-svg (svg factor)
  "Scale SVG by FACTOR."
  (let ((update-fn (-compose #'prettify-math--nu-to-string
                             (-rpartial #'prettify-math--scale-nu factor)
                             #'prettify-math--string-to-nu)))
    (prettify-math--dom-update-attr svg 'width update-fn)
    (prettify-math--dom-update-attr svg 'height update-fn)
    svg))

(defun prettify-math--ensure-defs (svg)
  "Ensure defs available in SVG."
  (or (dom-by-tag svg 'defs)
      (--doto (dom-node 'defs)
        (dom-add-child-before svg it)))
  svg)

(defun prettify-math--ensure-style (svg)
  "Ensure style available in SVG."
  (or (dom-by-tag svg 'style)
      (progn (prettify-math--ensure-defs svg)
             (--doto (dom-node 'style)
               (dom-add-child-before (dom-by-tag svg 'defs)
                                     it))))
  svg)

(defun prettify-math--color-to-rgb (c)
  "Color C to #rgb."
  (if (color-defined-p c)
      (--> c
           (color-values it)
           (mapcar (lambda (c) (ash c -8)) it)
           (apply #'format "#%02x%02x%02x" it))
    c))

;;;;;;;;;;;;;;;;;;; end mathjax ;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;; delimiter & utils ;;;;;;;;;;;;;;;;;
;; can't use customize, as init depdent on it
(defvar prettify-math-delimiters-alist
  '(("$" tex)
    ("$$" tex block)
    ("`" asciimath)
    ("``" asciimath block))
  "Delimiter is a string or cons of string.")

(defun prettify-math-block-delimiters ()
  "Block delimiters in delimiters-alist."
  (--keep (and (equal 'block (caddr it))
               (car it))
          prettify-math-delimiters-alist))

(defun prettify-math-contains-block-delimiters-p ()
  "Whether block delimiters available in delimiters-alist."
  (consp (prettify-math-block-delimiters)))

(defun prettify-math-delimiter-beg (delimiter)
  "DELIMITER itself or car DELIMITER."
  (cond ((consp delimiter) (car delimiter))
        ((stringp delimiter) delimiter)))

(defun prettify-math-delimiter-end (delimiter)
  "DELIMITER itself or cdr DELIMITER."
  (cond ((consp delimiter) (cdr delimiter))
        ((stringp delimiter) delimiter)))

(defun prettify-math-type-by-delimiter-beg (delimiter-beg)
  "Delimiter type by DELIMITER-BEG."
  (car (--keep (and  (equal (prettify-math-delimiter-beg (car it)) delimiter-beg)
                     (cadr it))
               prettify-math-delimiters-alist)))

(defun prettify-math--delimiter-to-regexp (delimiter &optional block)
  "Regexp (BLOCK) for expression inside DELIMITER."
  (let* ((dlmt-beginning (prettify-math-delimiter-beg delimiter))
         (dlmt-end (prettify-math-delimiter-end delimiter)))
    (concat "\\("
            (regexp-quote dlmt-beginning)
            "\\)"
            "\\("
            (if block
                "[^b-a]+?"
              ".+?")
            "\\)"
            (regexp-quote dlmt-end))))


;;;;;;;;;;;;;;;;;;; end delimiter & utils ;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;; fontlock related ;;;;;;;;;;;;;;;;;
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
      ;; scale not change resolution, so may blur image
      `(face nil display (,(--> dlmt
                                (prettify-math-type-by-delimiter-beg it)
                                (prettify-math--mathexp-to-svg mathexp it)
                                (prettify-math--to-dom it)
                                (prettify-math--scale-svg it (*  1.5
                                                    (or (--> face-remapping-alist
                                                             (assoc-default 'default it)
                                                             (assoc-default :height it)
                                                             car)
                                                        1)))
                                (-doto it
                                  (dom-set-attribute 'color (prettify-math--color-to-rgb (foreground-color-at-point))))
                                (svg-image it :ascent 'center)))
             cursor-sensor-functions (prettify-math--update-focus-on)
             rear-nonsticky (cursor-sensor-functions)))))

(defvar prettify-math--keywords
  (--map (list (prettify-math--delimiter-to-regexp (car it) (caddr it))
               0
               '(prettify-math--facespec-fn))
         prettify-math-delimiters-alist))

(defvar prettify-math--extra-properties
  '(display cursor-sensor-functions rear-nonsticky))

(defun prettify-math-extend-block-delimiter-region ()
  "Extend region from previous block dlm end or bob to next block dlm beg."
  (let* ((changed nil)
         (bdlms (prettify-math-block-delimiters))
         (bdlm-begs (-map #'prettify-math-delimiter-beg bdlms))
         (bdlm-begs-regexp (regexp-opt bdlm-begs))
         (bdlm-ends (-map #'prettify-math-delimiter-end bdlms))
         (bdlm-ends-regexp (regexp-opt bdlm-ends)))
    (save-excursion
      (save-match-data
        (if (boundp 'font-lock-beg)
            (progn
              (goto-char font-lock-beg)
              (re-search-backward bdlm-ends-regexp (point-min) t)
              (unless (equal (point) font-lock-beg)
                (setq changed t
                      font-lock-beg (point)))))
        (if (boundp 'font-lock-end)
            (progn
              (goto-char font-lock-end)
              (re-search-forward bdlm-begs-regexp (point-max) t)
              (unless (equal (point) font-lock-end)
                (setq changed t
                      font-lock-end (point)))))))
    changed))

;;;;;;;;;;;;;;;;;;; end fontlock related ;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;; mode related ;;;;;;;;;;;;;;;;;
(defun prettify-math--register-in-font-lock ()
  "Only keyword is suitble here.
As syntax class is mostly exclusive."
  (cursor-sensor-mode 1)
  (setq pre-redisplay-functions (delq 'cursor-sensor--detect pre-redisplay-functions))
  (when (prettify-math-contains-block-delimiters-p)
    (setq font-lock-multiline t)
    (add-hook 'font-lock-extend-region-functions #'prettify-math-extend-block-delimiter-region))
  (font-lock-add-keywords nil prettify-math--keywords)
  (--> prettify-math--extra-properties
       (append it font-lock-extra-managed-props)
       (setq font-lock-extra-managed-props it)))

(defun prettify-math--unregister-in-font-lock ()
  "Remove keywords."
  (cursor-sensor-mode -1)
  (when (prettify-math-contains-block-delimiters-p)
    (setq font-lock-multiline nil)
    (remove-hook 'font-lock-extend-region-functions #'prettify-math-extend-block-delimiter-region))
  (font-lock-remove-keywords nil prettify-math--keywords)
  (setq font-lock-extra-managed-props (--remove (memq it prettify-math--extra-properties)
                                        font-lock-extra-managed-props)))


;;;###autoload
(define-minor-mode prettify-math-mode
  "prettify math mode base on font lock"
  :lighter " pmath"
  (if (and (display-images-p)
           (image-type-available-p 'svg))
      (if prettify-math-mode
          (progn
            (prettify-math--register-in-font-lock)
            (font-lock-flush))
        (prettify-math--unregister-in-font-lock)
        (with-silent-modifications
          (remove-list-of-text-properties (point-min) (point-max)
                                          (cons 'focus-on prettify-math--extra-properties))))
    (warn "Display image (of svg) not supported")))

;;;###autoload
(define-globalized-minor-mode global-prettify-math-mode
  prettify-math-mode
  prettify-math-mode)
;;;;;;;;;;;;;;;;;;; end mode related ;;;;;;;;;;;;;;;;;

(provide 'prettify-math)

;;; prettify-math.el ends here
