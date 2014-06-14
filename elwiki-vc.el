;;; elwiki-vc.el --- The VCS wrapper for Elwiki.  -*- lexical-binding: t -*-

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

(require 'htmlize)
(require 'esxml)

(defun elwiki/out->stream (out-stream output)
  "Deal with the decision logic for an OUT-STREAM."
  (cond
    ((functionp out-stream)
     (funcall out-stream output))
    ;; This should cover all the other cases?
    ((not (eq output :eof))
     (print output out-stream))))

(defun elwiki/log->alist (commit-log)
  "Generate an alist from a commit-log line."
  (let ((seperator-count (count 0 commit-log)))
    (when (not (= 3 seperator-count))
      ;; There should be four fields, separated by 3 NULs.
      (error "Wrong number of fields: %s" (1+ seperator-count))))
  (mapcar*
   (lambda (k v)
     (cons k (htmlize-protect-string v)))
   '(hash date author subject)
   (split-string commit-log "\0")))

(defun elwiki/log-alist->esxml (commit-log)
  "Transform an alist commit log to ESXML.

Expects input to be the output of `elwiki/log->alist'."
  `(li ()
    ,@(kvmap-bind (class &rest field)
         `(span ((class . ,(symbol-name class)))
                ,(if (string= "hash" class)
                     (esxml-link (concat "?rev=" field)
                                 field)
                   field))
       commit-log)))

(defun elwiki/log-filter (process output out-stream)
  "Filter function for git-log process.

OUT-STREAM is where to send the log output, see
`elwiki/commit-log' for more details."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (save-excursion
        (goto-char (process-mark process))
        (insert output)
        (set-marker (process-mark process) (point))
        ;; Sent all complete lines, passing them through
        ;; `elwiki/log->alist'.
        (goto-char (point-min))
        (while (search-forward "\n" nil t nil)
          (elwiki/out->stream
           out-stream
           (esxml-to-xml
            (elwiki/log-alist->esxml
             (elwiki/log->alist
              (delete-and-extract-region (point-min) (1- (point)))))))
          ;; Delete the trailing newline.
          (delete-char -1))))))

(defun elwiki/log-sentinel (process event out-stream)
  "Sentinel for git-log.

OUT-STREAM is where to send the log output, see
`elwiki/commit-log' for more details."
  (if (string= "finished\n" event)
      ;; Finish the page and connection, and clean up if the process
      ;; exited without error.
      (progn
        ;; possibly we have a race condition here
        (elwiki/out->stream out-stream "</ul></body></html>")
        (elwiki/out->stream out-stream :eof)
        (kill-buffer (process-buffer process)))
      ;; Send an error message if it didn't.
      (message "An error occurred while retrieving the file history.")))

(defun* elwiki/commit-log (file number-of-commits skip-commits
                          &optional (out-stream t))
  "Get the last NUMBER-OF-COMMITS commits of FILE.

Skips the first SKIP-COMMITS commits.

Any HTML in the fields is escaped.

Sends a list of commits as alists of the form

  ((hash . \"xxxxxxx\")
   (date . \"yyyy-dd-mm hh:mm:ss +TZ\")
   (author . \"John Smith\")
   (subject . \"commit subject line\"))

to OUT-STREAM.

OUT-STREAM is an optional destination for the commit-logs, this can
be a buffer or a function or `nil' or `t'.  If a function it is
called with a single argument which is either string data or the
symbol `:eof' when the log ends.  If OUTPUT is nil or `t' then
the log is sent to the default output stream.

Returns the git process."
  ;; Get the date, author and subject, delimited by the null
  ;; character, of the next n commits.
  (let ((default-directory (file-name-directory file))
        (git-log-process
         (start-process
          "git-log" (generate-new-buffer-name "*git-log*")
          "git" "log"
          (format "--skip=%d" skip-commits)
          (format "-%d" number-of-commits)
          "--pretty=tformat:%h%x00%ci%x00%an%x00%s" file)))
    (set-process-filter
     git-log-process
     (lambda (proc data)
       (elwiki/log-filter proc data out-stream)))
    (set-process-sentinel
     git-log-process
     (lambda (proc status)
       (elwiki/log-sentinel proc status out-stream)))
    (elwiki/out->stream out-stream "<html><body><ul>")
    git-log-process))

(defun elwiki/http-commit-log (httpcon wikipage)
  (let* ((page (string-to-int (or (elnode-http-param httpcon "page") "")))
         (number-of-commits (string-to-int
                             (or (elnode-http-param httpcon "commits")
                                 "10"))) ; Default to 10.
         (skip-commits (* page number-of-commits)))
    (process-put
     httpcon :elnode-child-process
     (elwiki/commit-log
      wikipage number-of-commits skip-commits
      (lambda (data)
        (if (eq data :eof)
            (elnode-http-return httpcon)
          ;; Else send the data
          (elnode-http-send-string httpcon data)))))))

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
