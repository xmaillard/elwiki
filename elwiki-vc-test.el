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
(require 'elwiki-vc)

(ert-deftest elwiki/log->alist ()
  "Test `elwiki/log->alist'.

Tests with a valid commit-log string, one with too few fields,
and one with too many fields."
  (should (equal
           '((hash . "xxxxxxx")
             (date . "YYYY-MM-DD HH:MM:SS +TZ")
             (author . "John Smith")
             (subject . "commit subject"))
           (elwiki/log->alist
            "xxxxxxx\0YYYY-MM-DD HH:MM:SS +TZ\0John Smith\0commit subject")))
  (should-error (elwiki/log->alist "foo\0bar\0"))
  (should-error (elwiki/log->alist "foo\0bar\0goo\0foo\0")))

(ert-deftest elwiki/log-alist->esxml ()
  "Test `elwiki/log-alist->esxml'.

Tests with a valid commit-log alist."
  (should (equal
           '(li nil
              (span ((class . "hash"))
                (a ((href . "?rev=xxxxxxx")) "xxxxxxx"))
              (span ((class . "date"))
                "YYYY-MM-DD HH:MM:SS +TZ")
              (span ((class . "author"))
                "John Smith")
              (span ((class . "subject"))
                "commit subject"))
           (elwiki/log-alist->esxml '((hash . "xxxxxxx")
                                      (date . "YYYY-MM-DD HH:MM:SS +TZ")
                                      (author . "John Smith")
                                      (subject . "commit subject"))))))

(ert-deftest elwiki/commit-log ()
  "Test `elwiki/commit-log'.

Does not do a rigorous check on each field, just a sanity check."
  (let* ((temp-buffer (generate-new-buffer "*elwiki/commit-log test tmp*"))
         (git-proc (elwiki/commit-log "wiki/example.creole" 5 0
                     (lambda (x)
                       (unless (eq x :eof)
                         (with-current-buffer temp-buffer
                           (insert x)))))))
    ;; Wait for the git process to exit.
    (while (not (eq (process-status git-proc) 'exit))
      (sleep-for 1))
    ;; Check the `elwiki/commit-log' output.
    (should (string-match
             (rx "<html><body><ul>"
                 (repeat 5
                   (and "<li><span class=\"hash\"><a href=\"?rev="
                        (= 7 hex-digit) "\">"
                        (= 7 hex-digit)
                        "</a></span><span class=\"date\">"
                        (= 4 digit) ?- (= 2 digit) ?- (= 2 digit) " "
                        (= 2 digit) ?: (= 2 digit) ?: (= 2 digit)
                        " +" (= 4 digit)
                        "</span><span class=\"author\">"
                        (1+ printing)
                        "</span><span class=\"subject\">"
                        (1+ printing)
                        "</span></li>"))
                 "</ul></body></html>")
             (with-current-buffer temp-buffer
               (buffer-string))))))

;;; elwiki-vc-test.el ends here
