;;; vitamined-mode-line.el --- My own minimal modeline    -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Damien Pollet

;; Author: Damien Pollet <damien.pollet@gmail.com>
;; Keywords: local

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

;; TODO

;;; Code:

(defface mode-line-directory-face
  '((t (:inherit mode-line-buffer-id)))
  "File path mode line face")

(defface mode-line-filename-face
  '((t (:inherit mode-line-buffer-id)))
  "File name mode line face")

(defface mode-line-position-face
  '((t (:inherit mode-line-face)))
  "Buffer position indicator face")

(defface mode-line-status-face
  '((t (:inherit mode-line-face)))
  "General face for buffer status indicators")

(defface mode-line-modified-face
  '((t (:inherit mode-line-status-face)))
  "Face for buffer modified status indicator")

(defface mode-line-readonly-face
  '((t (:inherit mode-line-status-face)))
  "Face for buffer read-only status indicator")

(defface mode-line-narrowed-face
  '((t (:inherit mode-line-status-face)))
  "Face for buffer narrowed status indicator")

(defface mode-line-mode-face
  '((t (:inherit mode-line-face)))
  "Major mode indicator face")

(defface mode-line-minor-mode-face
  '((t (:inherit mode-line-mode-face)))
  "Minor mode indicator face")

(defface mode-line-process-face
  '((t (:inherit mode-line-face)))
  "Mode line process face")


(setq-default mode-line-format
              '("%e" mode-line-front-space

                ;; line:column position of point in buffer
                (line-number-mode
                 ((:propertize "%4l" face mode-line-position-face weight bold)
                  (column-number-mode
                   (:propertize ":%3c" face mode-line-position-face))))

                " "
                ;; narrowing indicator
                (:eval
                 (cond ((buffer-narrowed-p)
                        (propertize " ‡ " 'face 'mode-line-narrowed-face))
                       (t "   ")))


                ;; directory and buffer/file name
                ;; if uniquify, relies on the forward setting
                (:propertize
                 (" "
                  (:propertize (:eval (file-name-directory (buffer-name)))
                               face mode-line-directory-face)
                  (:eval (file-name-nondirectory (buffer-name)))
                  " ")
                 face mode-line-filename-face)

                ;; emacsclient indicator
                mode-line-client

                ;; read-only or modified status
                (:eval
                 (cond ((or view-mode buffer-read-only)
                        (propertize " × " 'face 'mode-line-readonly-face))
                       ((buffer-modified-p)
                        (propertize " ⁒ " 'face 'mode-line-modified-face))
                       (t "   ")))

                ;; mode indicators: vc, recursive edit, major mode, minor modes, process, global
                (vc-mode vc-mode)
                " %["
                (:propertize mode-name face mode-line-mode-face)
                "%] "
                (:propertize (:eval (format-mode-line minor-mode-alist))
                             face mode-line-minor-mode-face)
                (:propertize mode-line-process face mode-line-process-face)
                (global-mode-string global-mode-string)
                mode-line-end-spaces))

(provide 'vitamined-mode-line)
;;; vitamined-mode-line.el ends here
