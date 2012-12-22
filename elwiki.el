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

(require 'elnode)
(eval-when-compile 'fakir)
(require 'creole nil 't)

(defgroup elwiki nil
  "A Wiki server written with Elnode."
  :group 'elnode)

;;;###autoload
(defcustom elwiki-wikiroot
  "/tmp/elwiki/"
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

(defun elwiki--call (out-buf page-text page)
  "Call a wiki page sending output OUT-BUF.

The page is faked with PAGE-TEXT."
  (flet
      ((elnode--worker-lisp-helper (child-lisp)
         `((progn
             (require 'creole)
             (require 'cl)
             (flet ((creole--get-file (filename)
                      (let ((buf (get-buffer-create "wikibuf")))
                        (with-current-buffer buf
                          (insert ,page-text))
                        buf)))
               ,@child-lisp)))))
    (elnode-wait-for-exit
     (elnode-worker-elisp
         out-buf
         ((target page)
          (page-info page)
          (header elwiki-body-header)
          (footer elwiki-body-footer))
       (require 'creole)
       (creole-wiki
        target
        :destination t
        :variables `((page . ,page-info))
        :body-header header
        :body-footer footer)))))

(defun elwiki-page (httpcon wikipage &optional pageinfo)
  "Creole render a WIKIPAGE back to the HTTPCON."
      (elnode-http-start httpcon 200 `("Content-type" . "text/html"))
      (with-stdout-to-elnode httpcon
          (let ((page-info (or pageinfo (elnode-http-pathinfo httpcon)))
                (header elwiki-body-header)
                (footer elwiki-body-footer))
            (creole-wiki
             wikipage
             :destination t
             :variables (list (cons 'page page-info))
             :body-header header
             :body-footer footer))))

(defun elwiki--text-param (httpcon)
  "Get the text param from HTTPCON and convert it."
  (replace-regexp-in-string
   "\r" "" ; browsers send text in DOS line ending format
   (elnode-http-param httpcon "wikitext")))

(defun elwiki--save-request (httpcon wikiroot path text)
  "Process an update request."
  (let* ((page (if path
                   (save-match-data
                     (string-match "/wiki/\\(.*\\)$" path)
                     (match-string 1 path))))
         (comment (elnode-http-param httpcon "comment"))
         (file-name (if (equal page "")
                        (concat wikiroot "index.creole")
                      (concat (file-name-as-directory wikiroot) page)))
         (buffer (find-file-noselect file-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert text)
      (save-buffer)
      (let ((git-buf
             (get-buffer-create
              (generate-new-buffer-name
               "* elnode wiki commit buf *"))))
        (shell-command
         (format "git commit -m '%s' %s" comment file-name)
         git-buf)
        (kill-buffer git-buf))
      (elwiki-page httpcon file-name))))

(defun elwiki--router (httpcon)
  "Dispatch to a handler based on the URL."

  (elnode-hostpath-dispatcher httpcon
     `(("^[^/]+//wiki/\\(.*\\)" . elwiki--handler))))

(defun elwiki--handler (httpcon)
  "A low level handler for Wiki operations.

Send the Wiki page requested, which must be a file existing under
ELWIKI-WIKIROOT, back to the HTTPCON.

Update operations are NOT protected by authentication.  Soft
security is used."
  (let ((targetfile (elnode-http-mapping httpcon 1)))
   (flet ((elnode-http-mapping (httpcon which)
            (concat targetfile ".creole")))
     (elnode-method httpcon
       (GET
        (elnode-docroot-for (concat elwiki-wikiroot "/wiki/")
          with target-path
          on httpcon
          do
          (progn
            (message target-path)
            (elwiki-page httpcon target-path))))
       (POST
        (let* ((path (elnode-http-pathinfo httpcon))
               (text (elwiki--text-param httpcon)))
          (if (not (elnode-http-param httpcon "preview"))
              ;; A save request in which case save the new text and then
              ;; send the wiki text.
              (elwiki--save-request httpcon wikiroot path text)
            ;; Might be a preview request in which case send back the WIKI
            ;; text that's been sent.
            (with-temp-file "/tmp/preview"
              (insert text))
            (elwiki-send httpcon "/tmp/preview" path))))))))

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


;;; Tests

(ert-deftest elwiki-page ()
  "Full stack Wiki test."
  (with-elnode-mock-server
      ;; The dispatcher function
      (lambda (httpcon)
        (let ((elwiki-wikiroot "/home/elnode/wiki"))
          (elnode-hostpath-dispatcher
           httpcon
           '(("[^/]*//wiki/\\(.*\\)" . elwiki))))) t
    (fakir-mock-file (fakir-file
                      :filename "test.creole"
                      :directory "/home/elnode/wiki"
                      :content "= Hello World =\nthis is a creole wiki file!\n")
        (let* ((elnode--do-error-logging nil)
               (elnode--do-access-logging-on-dispatch nil))
          (should-elnode-response
           (elnode-test-call "/wiki/test.creole")
           :status-code 200
           :body-match ".*<h1>Hello World</h1>.*")))))

(provide 'elwiki)

;;; elwiki.el ends here
