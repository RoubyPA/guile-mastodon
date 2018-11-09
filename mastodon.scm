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
            get-current-account
            get-account-by-id
            new-status))

(define guile-mastodon-version "0.0.1")

(define (get-current-account inst)
  "Return current account of INST.

This function need valid token."
  (if (not (string= "" (instance-token inst)))
      (let ((my-account (mtd-accounts-verify-credentials inst)))
        (hashtab->account my-account))
      (throw 'mastodon                  ;
             `("error" . "Invalid token"))))

(define (get-account-by-id inst id)
  "Return account by ID on INST."
  (hashtab->account (mtd-accounts-by-id inst id)))

(define* (new-status inst #:optional #:key
                     (status         "")
                     (in-reply-to-id "")
                     (media-ids      '())
                     (sensitive      #f)
                     (spoiler-text   "")
                     (visibility     "public")
                     (language       ""))
  "Post new status on INST.

This function need valid token."
  ;; TODO: Add media-ids in args
  (let ((args (cons* (if (not (string= status ""))
                         `("status" ,status))
                     (if (not (string= in-reply-to-id ""))
                         `("in_reply_to_id" ,in-reply-to-id))
                     (if sensitive
                         '("sensitive" "true"))
                     (if (not (string= spoiler-text ""))
                         `("spoiler_text" ,spoiler-text))
                     (if (not (string= language ""))
                         `("language" ,language))
                     ;; TODO test visibility is correct
                     `(("visibility" ,visibility)))))
    (hashtab->status (mtd-new-status inst
                                     (remove (λ (a)
                                               (not (list? a)))
                                             args)))))
