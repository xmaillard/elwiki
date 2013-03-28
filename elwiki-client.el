;;; elwiki-client.el --- client for elwiki -*- lexical-binding: t -*-

;; Copyright (C) 2013  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: hypermedia

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

;; A very simple elwiki client for Emacs. Depends on creole-mode and
;; emacs-web.

;;; Code:

(require 'web)
(require 'creole-mode)

;;;###autoload
(defun elwikic (page)
  (interactive "Mpage: ")
  (web-http-get
   (lambda (httpc hdr data)
     (with-current-buffer (get-buffer-create (format "*wiki-%s*" page))
       (erase-buffer)
       (goto-char (point-max))
       (insert data)
       (decode-coding-region (point-min) (point-max) 'utf-8)
       (creole-mode)
       (view-mode t)
       (visual-line-mode t)
       (goto-char (point-min))
       (switch-to-buffer (current-buffer))))
   :url (format "http://localhost:8019/wiki/%s?raw=t" page)))


(provide 'elwiki-client)

;;; elwiki-client.el ends here
