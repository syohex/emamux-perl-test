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

(defun emamux-pt:top-directory-p (dir)
  (let ((default-directory dir))
    (loop for file in '("Makefile.PL" "Build.PL" ".proverc")
          when (file-exists-p file)
          return t)))

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

(defun emamux-pt:test-all-command (build-type topdir)
  (case build-type
    (makefile "make test")
    (build    "./Build test")
    (prove    (emamux-pt:test-all-prove topdir))
    (otherwise
     (error "Never reach here"))))

(defun emamux-pt:test-all-prove (topdir)
  (let ((default-directory topdir))
    (let ((t-dir-p  (file-directory-p "t/"))
          (xt-dir-p (file-directory-p "xt/")))
      (unless (or t-dir-p xt-dir-p)
        (error "No tests in your project!!"))
      (format "prove -v %s -r %s %s"
             (emamux-pt:lib-option topdir)
             (or (and t-dir-p "t/")  "")
             (or (and xt-dir-p "xt/") "")))))

(defun emamux-pt:test-file-command (file topdir)
  (format "prove -v %s %s" (emamux-pt:lib-option topdir) file))

(defun emamux-pt:lib-option (topdir)
  (let ((default-directory topdir))
    (cond ((file-directory-p "blib") "-b")
          ((file-directory-p "lib")  "-l")
          (t ""))))

(defun emamux-pt:concat-command (build-cmd test-cmd)
  (format " %s %s"
          (or (and build-cmd (concat build-cmd " && ")) "")
          test-cmd))

(defun emamux-pt:pre-command (type topdir)
  (let ((default-directory topdir))
    (case type
      (makefile  "perl Makefile.PL")
      (build     "perl Build.PL")
      (otherwise nil))))

(defun emamux-pt:built-p (type topdir)
  (let ((default-directory topdir))
    (or (and current-prefix-arg t)
        (case type
          (makefile (not (file-exists-p "Makefile")))
          (build    (not (file-exists-p "Build")))
          (otherwise nil)))))

(defun emamux-pt:test-command (test-type build-type file topdir)
  (if (eq test-type 'all)
      (emamux-pt:test-all-command build-type topdir)
    (emamux-pt:test-file-command file topdir)))

(defun emamux-pt:build-type (topdir)
  (let ((default-directory topdir))
    (cond ((file-exists-p "Makefile.PL") 'makefile)
          ((file-exists-p "Build.PL")    'build)
          (t 'prove))))

(defun emamux-pt:run-test-common (test-type)
  (let* ((topdir (emamux-pt:find-top-dir))
         (build-type (emamux-pt:build-type topdir))
         (built-p (emamux-pt:built-p build-type topdir))
         (build-cmd (and built-p (emamux-pt:pre-command build-type topdir)))
         (file (file-relative-name (buffer-file-name) topdir))
         (cmd (emamux-pt:test-command test-type build-type file topdir)))
    (emamux:run-command (emamux-pt:concat-command build-cmd cmd) topdir)))

(defun emamux-perl-test:run-all ()
  (interactive)
  (emamux-pt:run-test-common 'all))

(defun emamux-perl-test:run-this-file ()
  (interactive)
  (emamux-pt:run-test-common 'file))

(provide 'emamux-perl-test)

;;; emamux-perl-test.el ends here
