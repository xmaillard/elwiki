;;; elwiki-captcha.el --- The captcha for Elwiki.  -*- lexical-binding: t -*-

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
;; This is the captcha module for Elwiki.  It uses reCAPTCHA as its
;; backend, but provides a somewhat abstract interface.

;;; Code:

(defcustom elwiki/recaptcha-public-key
  "pubkey"
  "The site's public reCAPTCHA key."
  :type 'string
  :group 'elwiki/captcha)

(defcustom elwiki/recaptcha-private-key
  "privkey"
  "The site's private reCAPTCHA key."
  :type 'string
  :group 'elwiki/captcha)

(defun elwiki/captcha-esxml ()
  "Generates the HTML for the captcha.

A <fieldset>, in esxml form, to be included in a
captcha-protected <form>."
  `(fieldset ()
             (script ((type . "text/javascript")
                      (src . ,(concat "http://www.google.com/recaptcha/api/challenge?k="
                                      elwiki/recaptcha-public-key)))
                     "")
             (noscript ()
                       (iframe ((src . ,(concat "http://www.google.com/recaptcha/api/noscript?k="
                                                elwiki/recaptcha-public-key))
                                (height . "300")
                                (width . "500")
                                (frameborder . "0"))
                               "")
                       (br ())
                       (textarea ((name . "recaptcha_challenge_field")
                                  (rows . "3")
                                  (cols . "40"))
                                 "")
                       (input ((type . "hidden")
                               (name . "recaptcha_response_field")
                               (value . "manual_challenge"))))))

(defvar elwiki/captcha-verification-url
  "http://www.google.com/recaptcha/api/verify"
  "The URL for the captcha verifier.")

(defun elwiki/captcha-verify (httpcon)
  "Validates a captcha response sent via HTTPCON."
  (let* ((remoteip (elnode-http-host httpcon :just-host t))
         (challenge (elnode-http-param httpcon "recaptcha_challenge_field"))
         (response (elnode-http-param httpcon "recaptcha_response_field"))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
         (url-request-data (format "privatekey=%s&remoteip=%s&challenge=%s&response=%s"
                                   elwiki/recaptcha-private-key remoteip challenge response))
         (result))
    (with-current-buffer (url-retrieve-synchronously elwiki/captcha-verification-url)
      (setq result
            (mapcar*
             (lambda (k v)
               (cons k v))
             '(correct-solution-p error-code)
             (split-string (buffer-string) "\n"))))
    (if (string= "true" (cdr (assoc 'correct-solution-p result)))
        t
      (elnode-error "reCAPTCHA returned error: %s"
                    (cdr (assoc 'error-code result)))
      nil)))

(provide 'elwiki-captcha)

;;; elwiki-captcha.el ends here
