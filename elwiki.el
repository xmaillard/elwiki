;;; elwiki.el --- An Elnode-powered wiki engine.  -*- lexical-binding: t -*-

;; Copyright (C) 2012  Nic Ferrier, Aidan Gauland

;; Author: Nic Ferrier <nferrier@ferrier.me.uk> and Aidan Gauland <aidalgol@no8wireless.co.nz>
;; Maintainer: Aidan Gauland <aidalgol@no8wireless.co.nz>
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

;;; Source code
;;
;; elnode's code can be found here:
;;   http://github.com/nicferrier/elnode

;;; Style note
;;
;; This codes uses the Emacs style of:
;;
;;    elwiki/private-function
;;
;; for private functions.

;;; Commentary:
;;
;; This is a Wiki Engine completely written in EmacsLisp, using Elnode
;; as a server and Creole as the Wiki text language.
;;


;;; Code:

(elnode-app elwiki-dir
    elwiki-vc creole esxml htmlize)

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

(defcustom elwiki-wiki-name
  "Elwiki"
  "The name of the wiki.

This is used to prefix the page title."
  :type 'string
  :group 'elwiki)

(defcustom elwiki-global-stylesheet
  "/static/style.css"
  "The filename of the stylesheet to use on all pages.

The path must be relative to `elwiki-wikiroot' and start with a
slash.  In most cases, the file should be in \"/static/\"."
  :type '(file)
  :group 'elwiki)

(defun elwiki/link-resolver (short-link)
  "Resolver for bare links in creole.

Bound to `creole-link-resolver-fn' when calling elwikicreole to
generate HTML from creole pages."
  (cond
   ((not (find ?. short-link)) ;; Only wiki page links have no extension.
    (format "/wiki/%s" short-link))
   ((string-match-p "^.*\\.\\(png\\)$" short-link)
    (concat "/static/" short-link))))

(defun elwiki/page-name (path)
  "Return the name of the wikipage at PATH."
  (file-name-sans-extension
   (file-name-nondirectory path)))

(defun elwiki/wiki-directory ()
  "Returns the wiki-page directory.

Returns the canonical path of the directory in which the wiki
pages are stored."
  (file-name-as-directory
   (concat (file-name-as-directory elwiki-wikiroot)
           "wiki/")))

(defun elwiki/site-header-or-footer (header-or-footer)
  "Return the site-wide creole header or footer to HTTPCON.

HEADER-OR-FOOTER must be either 'header or 'footer (specifying
which one of the header and footer files to send), otherwise an
error is raised.

If the header or footer file does not exist, nil is returned."

  (when (not (and (symbolp header-or-footer)
                  (member header-or-footer '(header footer))))
    (error "Expected 'header or 'footer for second argument, got %S" header-or-footer))
  (let ((wiki-header-or-footer-file (format "%s__%s.creole"
                                            (elwiki/wiki-directory)
                                            (symbol-name header-or-footer))))
    (when (file-exists-p wiki-header-or-footer-file)
      (with-temp-buffer
        (insert-file-contents wiki-header-or-footer-file)
        (with-current-buffer
            (let ((creole-link-resolver-fn 'elwiki/link-resolver))
             (creole-html (current-buffer) nil
                          :do-font-lock t))
          (buffer-string))))))

(defun* elwiki/render-page (httpcon wikipage pageinfo &key pre post)
  "Creole render a WIKIPAGE back to the HTTPCON.

PRE and POST are put before and after the rendered WIKIPAGE,
verbatim."
  (let ((page-name (if pageinfo
                       (elwiki/page-name pageinfo)
                     (elwiki/page-name wikipage))))
   (elnode-http-send-string httpcon "<html>")
   ;; Document head
   (elnode-http-send-string
    httpcon
    (esxml-to-xml
     (esxml-head (format "%s: %s" elwiki-wiki-name page-name)
       (link 'stylesheet
             "text/css"
             elwiki-global-stylesheet))))
   (elnode-http-send-string httpcon "<body>")
   ;; Site-wide header.
   (elnode-http-send-string httpcon (or (elwiki/site-header-or-footer 'header) ""))
   ;; Argument-passed header.
   (when pre
     (elnode-http-send-string httpcon pre))
   ;; Rendered creole page.
   (elnode-http-send-string
    httpcon
    (with-temp-buffer
      (insert-file-contents wikipage)
      (with-current-buffer
          (let ((creole-link-resolver-fn 'elwiki/link-resolver))
           (creole-html (current-buffer) nil
                        :do-font-lock t))
        (buffer-string))))
   ;; Argument-passed footer.
   (when post
     (elnode-http-send-string httpcon post))
   ;; Site-wide footer.
   (elnode-http-send-string httpcon (or (elwiki/site-header-or-footer 'footer) ""))
   (elnode-http-return httpcon "</body>\n</html>")))

(defun elwiki-page (httpcon wikipage &optional pageinfo)
  "Creole render a WIKIPAGE back to the HTTPCON."
  (let* ((commit (elnode-http-param httpcon "rev"))
         (page-buffer (when commit (elwiki/get-revision wikipage commit))))
    (if (and commit
             (not page-buffer))
        ;; A specific page revision was requested, but we failed to get it.
        (elnode-send-404 httpcon "No such page revision.")
      (progn
        (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
        (elwiki/render-page
         httpcon
         (or page-buffer wikipage)
         pageinfo
         :post (pp-esxml-to-xml
                `(div ((class . "actions"))
                      ,(esxml-link "?action=edit" "Edit this page")
                      ,(esxml-link "?action=history" "View page history"))))))
    (when page-buffer
      (kill-buffer page-buffer))))

(defun elwiki-edit-page (httpcon wikipage &optional pageinfo preview)
  "Return an editor for WIKIPAGE via HTTPCON."
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (let* ((pageinfo (or pageinfo (elnode-http-pathinfo httpcon)))
         (page-name (elwiki/page-name pageinfo))
         (comment (elnode-http-param httpcon "comment"))
         (username (elnode-http-param httpcon "username"))
         (editor
          (esxml-to-xml
           `(form
             ((action . ,(format "%s?action=edit" pageinfo))
              (method . "POST"))
             (fieldset ()
                       (legend () ,(format "Edit %s" (file-name-nondirectory pageinfo)))
                       (textarea ((cols . "80")
                                  (rows . "20")
                                  (name . "wikitext"))
                                 ,(if (file-exists-p wikipage)
                                      (with-temp-buffer
                                        (insert-file-contents wikipage)
                                        (buffer-string))
                                    ""))
                       (br ())
                       (label () "Edit comment:"
                              (input ((type . "text")
                                      (name . "comment")
                                      (value . ,(or comment "")))))
                       (br ())
                       (label () "Username:"
                              (input ((type . "text")
                                      (name . "username")
                                      (value . ,(or username "")))))
                       (br ())
                       (input ((type . "submit")
                               (name . "save")
                               (value . "save")))
                       (input ((type . "submit")
                               (name . "preview")
                               (value . "preview"))))))))
    (if preview
        (elwiki/render-page
         httpcon
         wikipage
         pageinfo
         :post (format "<div id=editor>%s</div>" editor))
      (elnode-send-html
       httpcon
       (esxml-to-xml
        `(html ()
               ,(esxml-head (format "%s: editing %s" elwiki-wiki-name page-name)
                  (link 'stylesheet
                        "text/css"
                        elwiki-global-stylesheet))
               (body ()
                     ,editor)))))))

(defun elwiki-history-page (httpcon wikipage)
  (elnode-error "Generating history page for %s" wikipage)
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (let ((page (string-to-int (or (elnode-http-param httpcon "page") "")))
        (commits-per-page 10))
   (elnode-send-html
    httpcon
    (pp-esxml-to-xml
     `(html ()
            (body ()
                  ,(esxml-listify
                    (mapcar
                     (lambda (commit)
                       (kvmap-bind (class &rest field)
                           `(div ((class . ,(symbol-name class)))
                                 ,(if (string= "hash" class)
                                      (esxml-link (concat "?rev=" field)
                                                  field)
                                    field))
                         commit))
                     (elwiki/commit-log wikipage
                                        commits-per-page
                                        (* page commits-per-page))))))))))

(defun elwiki/text-param (httpcon)
  "Get the text parameter from HTTPCON and convert the line endings."
  (replace-regexp-in-string
   "\r" "" ; browsers send text in DOS line ending format
   (elnode-http-param httpcon "wikitext")))

(defun elwiki/save-request (httpcon wikiroot path text)
  "Process a page-save request."
  (let* ((page-name (save-match-data
                      (string-match "/wiki/\\(.*\\)$" path)
                      (match-string 1 path)))
         (comment (elnode-http-param httpcon "comment"))
         (username (elnode-http-param httpcon "username"))
         (file-name (expand-file-name (concat (file-name-as-directory wikiroot)
                                              path ".creole")))
         (buffer (find-file-noselect file-name)))
    (elnode-error "Saving page %s, edited by %s" page-name username)
    (with-current-buffer buffer
      (erase-buffer)
      (insert text)
      (save-buffer)
      (elwiki/commit-page file-name username comment)
      (elnode-send-redirect httpcon path))))

(defun elwiki/page-not-found (httpcon target-file action)
  "Page Not Found (404) response handler for wiki pages."
  (if (eq 'edit action)
      (elwiki-edit-page httpcon target-file)
    (elnode-send-404 httpcon
      (esxml-to-xml "The page you requested does not exist.
You can <a href=\"?action=edit\">create it</a> if you wish."))))

(defun elwiki/router (httpcon)
  "Dispatch to a handler depending on the URL.

So, for example, a handler for wiki pages, a separate handler for
images, and so on."
  (let ((webserver (elnode-webserver-handler-maker
                    (concat elwiki-dir "/static/"))))
    (elnode-hostpath-dispatcher httpcon
     `(("^[^/]*//wiki/\\(.*\\)" . elwiki/handler)
       ("^[^/]*//static/\\(.*\\)$" . ,webserver)))))

(defun elwiki/handler (httpcon)
  "A low level handler for wiki operations.

Send the wiki page requested, which must be a file existing under
ELWIKI-WIKIROOT, back to the HTTPCON.  The extension \".creole\"
is appended to the page name requested, so the request should not
include the extension.

Update operations are NOT protected by authentication.  Soft
security is used."
  (let ((targetfile (elnode-http-mapping httpcon 1))
        (action (intern (or (elnode-http-param httpcon "action")
                            "none"))))
   (flet ((elnode-http-mapping (httpcon which)
            (concat targetfile ".creole"))
          (elnode-not-found (httpcon target-file)
            (elwiki/page-not-found httpcon target-file action)))
     (elnode-method httpcon
       (GET
        (elnode-docroot-for (concat elwiki-wikiroot "/wiki/")
          with target-path
          on httpcon
          do
          (case action
            (none
             (elwiki-page httpcon target-path))
            (edit
             (elwiki-edit-page httpcon target-path))
            (history
             (elwiki-history-page httpcon target-path)))))
       (POST
        (let ((path (elnode-http-pathinfo httpcon))
               (text (elwiki/text-param httpcon)))
          (cond
           ;; A save request in which case save the new text and then
           ;; send the wiki text.
           ((and (elnode-http-param httpcon "save")
                 (eq action 'edit))
            (elwiki/save-request httpcon elwiki-wikiroot path text))
           ;; A preview request in which case send back the WIKI text
           ;; that's been sent.
           ((and (elnode-http-param httpcon "preview")
                 (eq action 'edit))
            (let ((preview-file-name "/tmp/preview"))
              (with-temp-file preview-file-name
                (insert text))
              (elwiki-edit-page httpcon preview-file-name path t))))))))))

;;;###autoload
(defun elwiki-server (httpcon)
  "Serve wiki pages from `elwiki-wikiroot'.

HTTPCON is the request.

The wiki server is only available if the `creole' package is
provided. Otherwise it will just error."
  (if (not (featurep 'creole))
      (elnode-send-500 httpcon "The Emacs feature 'creole is required.")
    (elwiki/router httpcon)))

(provide 'elwiki)

;;; elwiki.el ends here
