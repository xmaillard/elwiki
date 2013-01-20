;;; elwiki-vc.el --- The VCS wrapper for Elwiki.  -*- lexical-binding: t -*-

;; Copyright (C) 2012  Nic Ferrier, Aidan Gauland

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Maintainer: Aidan Gauland <aidalgol@no8wireless.co.nz>
;; Created: 5th October 2010
;; Keywords: lisp, http, hypermedia

;; This file is NOT part of GNU Emacs.

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

;;; Style note
;;
;; This codes uses the Emacs style of:
;;
;;    elwiki/private-function
;;
;; for private functions.

;;; Commentary:
;;
;; This is the Git wrapper for Elwiki.  Any invocation of git must be
;; from within the working directory of the wiki in order to keep git
;; from getting confused in the case that the working directory of the
;; Emacs instance is in a different Git repository.

;;; Code:

(defun elwiki/commit-log (file number-of-commits skip-commits)
  "Get the last NUMBER-OF-COMMITS commits of FILE.

Skips the first SKIP-COMMITS commits.

Any HTML in the fields is escaped.

TODO: document output format"
 ;; Get the date, author and subject
 ;; (delimited by null) of the next n
 ;; commits.
  (mapcar
   (lambda (commit)
    (mapcar*
     (lambda (k v)
       (cons k (htmlize-protect-string v)))
     '(hash date author subject)
     (split-string commit "\000")))
   (split-string
    (let ((default-directory (file-name-directory file)))
      (shell-command-to-string
       (format "git log --skip=%d -%d --pretty=format:%%h%%x00%%ci%%x00%%an%%x00%%s %s"
               skip-commits
               number-of-commits
               file)))
    "\n")))

(defun elwiki/commit-page (file username comment)
  "Commit any changes to FILE.

USERNAME is the name of the wiki user who submitted the changes,
and COMMENT is the page-edit comment."
  (let ((commit-message-file (make-temp-file
                              (file-name-nondirectory file))))
    (with-temp-file commit-message-file
      (insert (format "username: %s\n" username))
      (insert comment))
    (with-temp-buffer
      (call-process "git"
                    nil
                    (current-buffer)
                    nil
                    "commit" "--dry-run"
                    (format "--file=%s" commit-message-file)
                    file))))

(defun elwiki/get-revision (file commit)
  "Get the version of FILE from COMMIT.

Returns the requested version of FILE in a buffer, or NIL if
there is no such file in COMMIT or there is no such COMMIT.

Only handles text files."
  (let* ((default-directory (file-name-directory file))
         (file-name-relateive (save-match-data
                                (string-match "/\\(wiki/.*\\)$" file)
                                (match-string 1 file)))
         (git-buffer (generate-new-buffer
                      (format "*%s:%s*" commit file-name-relateive))))
    (elnode-error "Running git show %s:%s" commit file-name-relateive)
    (let ((exit-status
           (call-process "git"
                         nil
                         git-buffer
                         nil
                         "show" (format "%s:%s" commit file-name-relateive))))
      (with-current-buffer git-buffer
        (elnode-error "git output: %s" (buffer-string)))
      (if (= 0 exit-status)
          git-buffer
        nil))))

(provide 'elwiki-vc)

;;; elwiki-vc.el ends here
