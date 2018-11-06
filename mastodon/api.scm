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
  #:use-module (web uri)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-11)
  #:use-module (json)
  #:export (mastodon-api-get
            mastodon-api-post
            mastodon-api-delete
            ;; Accounts
            mtd-accounts-by-id
            mtd-accounts-verify-credentials
            mtd-accounts-id-followers
            mtd-accounts-id-following
            mtd-accounts-id-statuses
            mtd-accounts-search
            ;; Instance
            mtd-instance-info
            ;; Statuses
            mtd-status-by-id
            mtd-status-context-by-id
            mtd-status-card-by-id
            mtd-status-id-reblogged-by
            mtd-status-id-favourited-by
            mtd-delete-status-by-id
            mtd-new-status
            mtd-status-id-reblog
            mtd-status-id-unreblog
            mtd-status-id-pin
            mtd-status-id-unpin))

;;;
;;; Method.
;;;

(define (mastodon-api-get request token)
  "Send http get request to mastodon instance. REQUEST is url of api, and
TOKEN is authentification token. Return hashtable of json response or throw an
error with `mastodon' tag."
  (let-values (((res body)
                (http-get request
                          #:body #f
                          #:version '(1 . 1)
                          #:keep-alive? #f
                          #:headers `((Authorization . ,(string-append "Bearer " token)))
                          #:decode-body? #t
                          #:streaming? #f)))
    (match (response-code res)
      (200
       (json-string->scm (bytevector->string body "utf-8")))
      (_
       ;; Error
       (throw 'mastodon `("response-code" . ,(response-code res)))))))

(define (mastodon-api-post request data token)
  "Send http get request to mastodon instance. REQUEST is url of api, and
TOKEN is authentification token. Return hashtable of json response or throw an
error with `mastodon' tag."
  (let-values (((res body)
                (http-post request
                           #:body (string->bytevector data "utf-8")
                           #:version '(1 . 1)
                           #:keep-alive? #f
                           #:headers `((Authorization . ,(string-append "Bearer " token)))
                           #:decode-body? #t
                           #:streaming? #f)))
    (match (response-code res)
      (200
       (json-string->scm (bytevector->string body "utf-8")))
      (_
       ;; Error
       (throw 'mastodon `("response-code" . ,(response-code res)))))))

(define (mastodon-api-delete request token)
  "Send http delete request to mastodon instance. REQUEST is url of api, and
TOKEN is authentification token. Return hashtable of json response or throw an
error with `mastodon' tag."
  (let-values (((res body)
                (http-delete request
                             #:body #f
                             #:version '(1 . 1)
                             #:keep-alive? #f
                             #:headers `((Authorization
                                          . ,(string-append "Bearer " token)))
                             #:decode-body? #t
                             #:streaming? #f)))
    (match (response-code res)
      (200
       (json-string->scm (bytevector->string body "utf-8")))
      (_
       ;; Error
       (throw 'mastodon `("response-code" . ,(response-code res)))))))

;;;
;;; Accounts.
;;;

(define (mtd-accounts-by-id instance id)
  "Send request to INSTANCE to get user information by user ID. Return the
hash-table of json response."
  (let ((url (string-append (instance-url instance)
                            "/api/v1/accounts/" id)))
    (mastodon-api-get url (instance-token instance))))

(define (mtd-accounts-verify-credentials instance)
  "Send request to INSTANCE to get current user information. Return the
hash-table of json response.
This feature need valid instance token."
  (let ((url (string-append (instance-url instance)
                            "/api/v1/accounts/verify_credentials")))
    (mastodon-api-get url (instance-token instance))))

(define (mtd-accounts-id-followers instance id)
  "Send request to INSTANCE to get user followers, user corresponding to
ID. Return the hash-table of json response.
This feature need valid instance token."
  (let ((url (string-append (instance-url instance)
                            "/api/v1/accounts/" id "/followers")))
    (mastodon-api-get url (instance-token instance))))

(define (mtd-accounts-id-following instance id)
  "Send request to INSTANCE to get user following, user corresponding to
ID. Return the hash-table of json response.
This feature need valid instance token."
  (let ((url (string-append (instance-url instance)
                            "/api/v1/accounts/" id "/following")))
    (mastodon-api-get url (instance-token instance))))

(define (mtd-accounts-id-statuses instance id)
  "Send request to INSTANCE to get user statuses, user corresponding to
ID. Return the hash-table of json response.
This feature need valid instance token."
  (let ((url (string-append (instance-url instance)
                            "/api/v1/accounts/" id "/statuses")))
    (mastodon-api-get url (instance-token instance))))

(define (mtd-accounts-search instance name)
  "Send request to INSTANCE to search accuonts by username, domain and display
name, user corresponding to NAME. Return the hash-table of json response.
This feature need valid instance token."
  (let ((url (string-append (instance-url instance)
                            "/api/v1/accounts/search?q=" name)))
    (mastodon-api-get url (instance-token instance))))


;;;
;;; Instance.
;;;

(define (mtd-instance-info instance)
  "Information about the server. Returns instance
'https://docs.joinmastodon.org/api/entities/#instance' as hash-table."
  (let ((url (string-append (instance-url instance)
                            "/api/v1/instance")))
    (mastodon-api-get url (instance-token instance))))

;;;
;;; Statuses.
;;;

(define (mtd-status-by-id instance id)
  "Get status coresponding to ID."
  (let ((url (string-append (instance-url instance)
                            "/api/v1/statuses/" id)))
    (mastodon-api-get url (instance-token instance))))

(define (mtd-status-context-by-id instance id)
  "Get status context coresponding to ID."
  (let ((url (string-append (instance-url instance)
                            "/api/v1/statuses/" id "/context")))
    (mastodon-api-get url (instance-token instance))))

(define (mtd-status-card-by-id instance id)
  "Get status card coresponding to ID."
  (let ((url (string-append (instance-url instance)
                            "/api/v1/statuses/" id "/card")))
    (mastodon-api-get url (instance-token instance))))

(define (mtd-status-id-reblogged-by instance id)
  "Get list of accounts who reblogged status coresponding to ID."
  (let ((url (string-append (instance-url instance)
                            "/api/v1/statuses/" id "/reblogged_by")))
    (mastodon-api-get url (instance-token instance))))

(define (mtd-status-id-favourited-by instance id)
  "Get list of accounts who favourited status coresponding to ID."
  (let ((url (string-append (instance-url instance)
                            "/api/v1/statuses/" id "/favourited_by")))
    (mastodon-api-get url (instance-token instance))))

(define (mtd-delete-status-by-id instance id)
  "Delete status coresponding to ID."
  (let ((url (string-append (instance-url instance)
                            "/api/v1/statuses/" id)))
    (mastodon-api-delete url (instance-token instance))))

(define (mtd-new-status instance args)
  "Post a new status on INSTANCE. ARGS is list of parameters. You need to
provide \"status\" or \"media_ids\", for more information see mastodon docs."
  (let ((url  (string-append (instance-url instance)
                             "/api/v1/statuses"))
        (data (apply string-append
                     (map (lambda (l)
                            (string-append (uri-encode (car  l)) "="
                                           (uri-encode (cadr l)) "&"))
                          args))))
    (mastodon-api-post url data
                       (instance-token instance))))

(define (mtd-status-id-reblog instance id)
  "Reblog status coresponding to ID."
  (let ((url (string-append (instance-url instance)
                            "/api/v1/statuses/" id "/reblog")))
    (mastodon-api-post url "" (instance-token instance))))

(define (mtd-status-id-unreblog instance id)
  "Unreblog status coresponding to ID."
  (let ((url (string-append (instance-url instance)
                            "/api/v1/statuses/" id "/unreblog")))
    (mastodon-api-post url "" (instance-token instance))))

(define (mtd-status-id-pin instance id)
  "Pin status coresponding to ID."
  (let ((url (string-append (instance-url instance)
                            "/api/v1/statuses/" id "/pin")))
    (mastodon-api-post url "" (instance-token instance))))

(define (mtd-status-id-unpin instance id)
  "Unpin status coresponding to ID."
  (let ((url (string-append (instance-url instance)
                            "/api/v1/statuses/" id "/unpin")))
    (mastodon-api-post url "" (instance-token instance))))
