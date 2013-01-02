;;; elwiki.el --- An Elnode-powered wiki engine.  -*- lexical-binding: t -*-

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

;;; Commentary:
;;
;; This is a Wiki Engine completely written in EmacsLisp, using Elnode
;; as a server.
;;
;;; Source code
;;
;; elnode's code can be found here:
;;   http://github.com/nicferrier/elnode

;;; Style note
;;
;; This codes uses the Emacs style of:
;;
;;    elwiki--private-function
;;
;; for private functions.


;;; Code:

(elnode-app elwiki-dir
    creole)

(defgroup elwiki nil
  "A Wiki server written with Elnode."
  :group 'elnode)

;;;###autoload
(defcustom elwiki-wikiroot
  elwiki-dir
  "The root for the Elnode wiki files.

This is where elwiki serves wiki files from.  You
should change this."
  :type '(directory)
  :group 'elwiki)

(defcustom elwiki-body-header
  "<div id='top'></div>"
  "HTML BODY preamable of a rendered Wiki page."
  :type '(string)
  :group 'elwiki)

(defcustom elwiki-body-footer
  "<div id='footer'></div>"
  "HTML BODY footter for a rendered Wiki page."
  :type '(string)
  :group 'elwiki)

(defun elwiki--render-page (wikipage pageinfo)
  "Creole render WIKIPAGE to stdout."
  (creole-wiki
   wikipage
   :destination t
   :variables (list (cons 'page pageinfo))
   :body-header elwiki-body-header
   :body-footer elwiki-body-footer))

(defun elwiki-page (httpcon wikipage &optional pageinfo)
  "Creole render a WIKIPAGE back to the HTTPCON."
      (elnode-http-start httpcon 200 `("Content-type" . "text/html"))
      (with-stdout-to-elnode httpcon
        (elwiki--render-page wikipage (or pageinfo
                                          (elnode-http-pathinfo httpcon)))))

(defun elwiki-edit-page (httpcon wikipage &optional pageinfo preview)
  "Return an editor for WIKIPAGE via HTTPCON."
  (elnode-http-start httpcon 200 `("Content-type" . "text/html"))
  (with-stdout-to-elnode httpcon
    (when preview
      (elwiki--render-page wikipage (or pageinfo
                                        (elnode-http-pathinfo httpcon))))
    (let ((page-info (or pageinfo (elnode-http-pathinfo httpcon))))
      (princ (format "<form action='%s' method='POST'>
<fieldset>
<legend>Edit %s</legend>
<textarea  cols='80' rows='20' name='wikitext'>"
                     page-info
                     (file-name-nondirectory page-info)))
      (with-temp-buffer
        (insert-file-contents wikipage)
        (princ (buffer-string)))
        (princ (format "</textarea><br/>
<label>Edit comment: <input type='text' name='comment' value=''/></label>
<input type='submit' name='save' value='save'/>
<input type='submit' name='preview' value='preview' formaction='%s?action=edit'/>
</fieldset>
</form>"
                       page-info)))))

(defun elwiki--text-param (httpcon)
  "Get the text param from HTTPCON and convert it."
  (replace-regexp-in-string
   "\r" "" ; browsers send text in DOS line ending format
   (elnode-http-param httpcon "wikitext")))

(defun elwiki--save-request (httpcon wikiroot path text)
  "Process an update request."
  (elnode-error "Saving page: %s" path)
  (let* ((page-name (save-match-data
                      (string-match "/wiki/\\(.*\\)$" path)
                      (match-string 1 path)))
         (comment (elnode-http-param httpcon "comment"))
         (file-name (expand-file-name (concat (file-name-as-directory wikiroot)
                                              path ".creole")))
         (buffer (find-file-noselect file-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert text)
      (save-buffer)
      ;; (let ((git-buf
      ;;        (get-buffer-create
      ;;         (generate-new-buffer-name
      ;;          "* elnode wiki commit buf *"))))
      ;;   (shell-command
      ;;    (format "git commit -m '%s' %s" comment file-name)
      ;;    git-buf)
      ;;   (kill-buffer git-buf))
      (elnode-send-redirect httpcon path))))

(defun elwiki--router (httpcon)
  "Dispatch to a handler based on the URL."
  (let ((webserver (elnode-webserver-handler-maker
                    (concat elwiki-dir "/static/"))))
    (elnode-hostpath-dispatcher httpcon
     `(("^[^/]*//wiki/\\(.*\\)" . elwiki--handler)
       ("^[^/]*//static/\\(.*\\)$" . ,webserver)))))

(defun elwiki--handler (httpcon)
  "A low level handler for Wiki operations.

Send the Wiki page requested, which must be a file existing under
ELWIKI-WIKIROOT, back to the HTTPCON.

Update operations are NOT protected by authentication.  Soft
security is used."
  (let ((targetfile (elnode-http-mapping httpcon 1))
        (action (intern (or (elnode-http-param httpcon "action")
                            "none"))))
   (flet ((elnode-http-mapping (httpcon which)
            (concat targetfile ".creole")))
     (elnode-method httpcon
       (GET
        (elnode-docroot-for (concat elwiki-wikiroot "/wiki/")
          with target-path
          on httpcon
          do
          (case action
           ((none)
            (elwiki-page httpcon target-path))
           ((edit)
            (elwiki-edit-page httpcon target-path)))))
       (POST
        (let ((path (elnode-http-pathinfo httpcon))
               (text (elwiki--text-param httpcon)))
          (cond
           ((elnode-http-param httpcon "save")
            ;; A save request in which case save the new text and then
            ;; send the wiki text.
            (elwiki--save-request httpcon elwiki-wikiroot path text))
           ((and (elnode-http-param httpcon "preview")
                 (eq action 'edit))
            ;; A preview request in which case send back the WIKI text
            ;; that's been sent.
            (with-temp-file "/tmp/preview"
              (insert text))
            (elwiki-edit-page httpcon "/tmp/preview" path t)))))))))

;;;###autoload
(defun elwiki-test ()
  "Test whether we should serve Wiki or not."
  (featurep 'creole))

;;;###autoload
(defun elwiki-server (httpcon)
  "Serve wiki pages from `elwiki-wikiroot'.

HTTPCON is the request.

The wiki server is only available if the `creole' package is
provided. Otherwise it will just error."
  (if (not (elwiki-test))
      (elnode-send-500 httpcon "The Emacs feature 'creole is required.")
    (elwiki--router httpcon)))

(provide 'elwiki)

;;; elwiki.el ends here
