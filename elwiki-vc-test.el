;;; elwiki-vc-test.el --- Elwiki tests  -*- lexical-binding: t -*-

;; Copyright (C) 2012  Nic Ferrier, Aidan Gauland

;; Author: Nic Ferrier <nferrier@ferrier.me.uk> and Aidan Gauland <aidalgol@amuri.net>
;; Maintainer: Aidan Gauland <aidalgol@amuri.net>
;; Created: 5th October 2010
;; Keywords: lisp, http, hypermedia

;; This file is NOT part of GNU Emacs.

;; This file is part of Elwiki.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; These are the ERT tests for Elwiki-VC.
;;


;;; Code:

(require 'ert)

(ert-deftest elwiki/log->alist ()
  "Test `elwiki/log->alist'."
  (should (equal
           '((hash . "xxxxxxx")
             (date . "YYYY-MM-DD HH:MM:SS +TZ")
             (author . "John Smith")
             (subject . "commit subject"))
           (elwiki/log->alist
            "xxxxxxx\0YYYY-MM-DD HH:MM:SS +TZ\0John Smith\0commit subject")))
  (should-error (elwiki/log->alist "foo\0bar\0"))
  (should-error (elwiki/log->alist "foo\0bar\0goo\0foo\0")))

;;; elwiki-vc-test.el ends here
