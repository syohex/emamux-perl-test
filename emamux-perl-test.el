;;; emamux-perl-test.el --- Perl test with emamux

;; Copyright (C) 2012 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emamux-perl-test

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; History:
;; Revision 0.1  2012/07/???? syohex
;; Initial version
;;

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'emamux)

(defvar emamux-pt:build-file-list
  '("Makefile.PL" "Build" ".proverc"))

(defvar emamux-pt:build-type nil)
(defvar emamux-pt:top-directory nil)

(defun emamux-pt:top-directory-p (dir)
  (let ((default-directory dir))
    (find-if (lambda (f)
               (file-exists-p f)) emamux-pt:build-file-list)))

(defun emamux-pt:init ()
  (setq emamux-pt:top-directory nil)
  (setq emamux-pt:build-type nil))

(defun emamux-pt:parent-dir (dir)
  (file-name-directory (replace-regexp-in-string "/$" "" dir)))

(defun emamux-pt:find-top-dir ()
  (let ((dir (file-name-directory (buffer-file-name))))
    (while (and (not (string= dir "/"))
                (not (emamux-pt:top-directory-p dir)))
      (setq dir (emamux-pt:parent-dir dir)))
    (if (string= dir "/")
        (error "Are you in Perl module directory ?"))
    (file-name-as-directory dir)))

(defun emamux-pt:test-all-command ()
  (case emamux-pt:build-type
    (makefile "make test")
    (build    "./Build test")
    (prove    (emamux-pt:test-all-prove))
    (otherwise
     (error "Never reach here"))))

(defun emamux-pt:test-all-prove ()
  (format "prove -v %s -r t/" (emamux-pt:lib-option)))

(defun emamux-pt:test-file-command (file)
  (format "prove -v %s %s" (emamux-pt:lib-option) file))

(defun emamux-pt:lib-option ()
  (let ((default-directory emamux-pt:top-directory))
    (cond ((file-directory-p "blib") "-b")
          ((file-directory-p "lib")  "-l")
          (t ""))))

(defun emamux-pt:get-build-type ()
  (let ((default-directory emamux-pt:top-directory))
    (cond ((file-exists-p "Makefile.PL") 'makefile)
          ((file-exists-p "Build.PL") 'build)
          (t 'prove))))

(defun emamux-pt:wrap-command (cmd)
  (format " (cd %s && %s)" emamux-pt:top-directory cmd))

(defun emamux-pt:run-test-common (type)
  (emamux-pt:init)
  (let ((emamux-pt:top-directory (emamux-pt:find-top-dir)))
    (setq emamux-pt:build-type (emamux-pt:get-build-type))
    (let ((cmd (if (eq type 'all)
                   (emamux-pt:test-all-command)
                 (emamux-pt:test-file-command (buffer-file-name)))))
      (emamux:run-command (emamux-pt:wrap-command cmd)))))

(defun emamux-perl-test:run-all ()
  (interactive)
  (emamux-pt:run-test-common 'all))

(defun emamux-perl-test:run-this-file ()
  (interactive)
  (emamux-pt:run-test-common 'file))

;;; emamux.el ends here
