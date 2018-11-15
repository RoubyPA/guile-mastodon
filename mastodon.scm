;;; mastodon.scm --- Guile module for mastodon.
;;
;; Copyright (C) 2018 by Pierre-Antoine Rouby <contact@parouby.fr>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>

(define-module (mastodon)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (mastodon api)
  #:use-module (mastodon types)
  #:export (guile-mastodon-version
            new-status))

(define guile-mastodon-version "0.0.1")

(define* (new-status inst #:optional #:key
                     (status         "")
                     (in-reply-to-id "")
                     (media-ids      '())
                     (sensitive      #f)
                     (spoiler-text   "")
                     (visibility     "public")
                     (language       "")
                     (auto-pinned    #f)
                     (auto-reblog    #f))
  "Post new status on mastodon instance INST."
  (let ((args (cons* (if (not (string= status ""))
                         `("status" . ,status)
                         #f)
                     (if (not (string= in-reply-to-id ""))
                         `("in_reply_to_id" . ,in-reply-to-id)
                         #f)
                     (if sensitive
                         '("sensitive" . "true")
                         #f)
                     (if (not (string= spoiler-text ""))
                         `("spoiler_text" . ,spoiler-text)
                         #f)
                     (if (not (string= language ""))
                         `("language" . ,language)
                         #f)
                     (if (not (equal? media-ids '()))
                         `("media_ids" . ,media-ids)
                         #f)
                     ;; TODO test visibility is correct
                     `(("visibility" . ,visibility)))))
    (let* ((new (mtd-new-status inst (remove not args)))
           (id (status-id new)))
      (when auto-pinned (mtd-status-id-pin inst id))
      (when auto-reblog (mtd-status-id-reblog inst id))
      ;; Return status
      new)))
