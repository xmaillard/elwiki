;;; elwiki-test.el --- Elwiki tests  -*- lexical-binding: t -*-

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

;;; Commentary:
;;
;; These are the ERT tests for Elwiki.
;;


;;; Code:

(require 'ert)

(ert-deftest elwiki/page-name ()
  "Test `elwiki/page-name'."
  (should (string= "pagename"
                   (elwiki/page-name "/path/to/wikiroot/pagename.creole")))
  (should (string= "pagename"
                   (elwiki/page-name "pagename.creole"))))

(ert-deftest elwiki/wiki-dir ()
  "Test `elwiki/wiki-dir'."
  (let ((elwiki-wikiroot "/path/to/wikiroot/"))
    (should (string=
             "/path/to/wikiroot/wiki/"
             (elwiki/wiki-directory))))
  (let ((elwiki-wikiroot "/path/to/wikiroot"))
    (should (string=
             "/path/to/wikiroot/wiki/"
             (elwiki/wiki-directory)))))

(ert-deftest elwiki/header-footer ()
  "Test `elwiki/send-site-header-or-footer'."
  ;; Should only accept 'header and 'footer.
  (should-error (elwiki/site-header-or-footer 'invalid))
  (should-error (elwiki/site-header-or-footer nil))
  (should-error (elwiki/site-header-or-footer t))
  (let ((elwiki-wikiroot "/path/to/wikiroot/"))
    ;; Should return nil if the file doesn't exist.
    (should (eq nil
                (elwiki/site-header-or-footer 'header)))
    (should (eq nil
                (elwiki/site-header-or-footer 'footer)))
    ;; Should return the file if it exists (tested with a mock file).
   (fakir-mock-file
       (fakir-file
        :filename "__header.creole"
        :directory "/path/to/wikiroot/wiki"
        :content "ERT header file\n")
     (should (file-exists-p "/path/to/wikiroot/wiki/__header.creole"))
     (should (string=
              "<p>ERT header file</p>\n"
              (elwiki/site-header-or-footer 'header))))))

(ert-deftest elwiki-page ()
  "Full stack Wiki test."
  (with-elnode-mock-server
      ;; The dispatcher function
      (lambda (httpcon)
        (let ((elwiki-wikiroot "/home/elnode/wiki"))
          (elnode-hostpath-dispatcher
           httpcon
           '(("[^/]*//wiki/\\(.*\\)" . elwiki/handler))))) t
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

;;; elwiki-test.el ends here
