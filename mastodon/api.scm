;;; api.scm --- Guile module for mastodon.
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

(define-module (mastodon api)
  #:use-module (mastodon)
  #:use-module (mastodon instance)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-11)
  #:use-module (json)
  #:export (mastodon-api-get
            mtd-accounts-by-id))

(define (mastodon-api-get request token)
  "Send http get request to mastodon instance. REQUEST is url of api, and
TOKEN is authentification token. Return hashtable of json response or throw an
error with `mastodon' tag."
  (let-values (((res body)
                (http-get request
                          #:body #f
                          #:version '(1 . 1)
                          #:keep-alive? #f
                          #:headers '()
                          #:decode-body? #t
                          #:streaming? #f)))
    (match (response-code res)
      (200
       (json-string->scm (bytevector->string body "utf-8")))
      (_
       ;; Error
       (throw 'mastodon `("response-code" . ,(response-code res)))))))

(define (mtd-accounts-by-id instance id)
  "Send request to INSTANCE to get user information by user ID. Return the
hash-table of json response."
  (let ((url (string-append (instance-url instance)
                            "/api/v1/accounts/" id)))
    (mastodon-api-get url (instance-token instance))))
