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
  #:use-module (mastodon types)
  #:use-module (web uri)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 match)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-11)
  #:use-module (json)
  #:export (mastodon-api-get
            mastodon-api-post
            mastodon-api-delete
            ;; Accounts
            mtd-accounts-by-id
            mtd-accounts-verify-credentials
            mtd-accounts-id-followers
            mtd-accounts-id-follow
            mtd-accounts-id-unfollow
            mtd-accounts-id-following
            mtd-accounts-id-statuses
            mtd-accounts-search
            ;; Apps
            mtd-apps-verify-credentials
            ;; Blocks
            mtd-account-blocked
            mtd-account-block
            mtd-account-unblock
            ;; Emoji
            mtd-custom-emojis
            ;; Domain blocks
            mtd-domain-blocked
            mtd-domain-block
            mtd-domain-unblock
            ;; Endorsements
            mtd-endorsed
            mtd-endorse
            mtd-unendorse
            ;; Favourites
            mtd-favourited
            mtd-favourite-status
            mtd-unfavourite-status
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
            mtd-status-id-unpin
            ;; Search
            mtd-search
            ;; Media
            mtd-post-media
            ;; Mutes
            mtd-muted
            mtd-mute
            mtd-unmute
            mtd-status-mute
            mtd-status-unmute))

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
                          #:headers `((Authorization
                                       . ,(string-append "Bearer " token)))
                          #:decode-body? #t
                          #:streaming? #f)))
    (match (response-code res)
      (200
       (json-string->scm (bytevector->string body "utf-8")))
      (_
       ;; Error
       (throw 'mastodon `(("response-code" . ,(response-code res))
                          ("response-phrase" .
                           ,(response-reason-phrase res))
                          ("response" .
                           ,(if (bytevector? body)
                                (bytevector->string body "utf-8")
                                body))))))))

(define* (mastodon-api-post request data token #:key
                            (content-type "application/x-www-form-urlencoded"))
  "Send http post request to mastodon instance. REQUEST is url of api, and
TOKEN is authentification token. Return hashtable of json response or throw an
error with `mastodon' tag."
  (let-values (((res body)
                (http-post request
                           #:body (if (bytevector? data)
                                      data
                                      (string->bytevector data "utf-8"))
                           #:version '(1 . 1)
                           #:keep-alive? #f
                           #:headers `((Authorization
                                        . ,(string-append "Bearer " token))
                                       (Content-Type . ,content-type))
                           #:decode-body? #t
                           #:streaming? #f)))
    (match (response-code res)
      (200
       (json-string->scm (bytevector->string body "utf-8")))
      (_
       ;; Error
       (throw 'mastodon `(("response-code" . ,(response-code res))
                          ("response-phrase" .
                           ,(response-reason-phrase res))
                          ("response" .
                           ,(if (bytevector? body)
                                (bytevector->string body "utf-8")
                                body))))))))

(define* (mastodon-api-delete request token #:key (data #f)
                              (content-type "application/x-www-form-urlencoded"))
  "Send http delete request to mastodon instance. REQUEST is url of api, and
TOKEN is authentification token. Return hashtable of json response or throw an
error with `mastodon' tag."
  (let-values (((res body)
                (http-delete request
                             #:body (if (string? data)
                                        (string->bytevector data "utf-8")
                                        data)
                             #:version '(1 . 1)
                             #:keep-alive? #f
                             #:headers `((Authorization
                                          . ,(string-append "Bearer " token))
                                         (Content-Type . ,content-type))
                             #:decode-body? #t
                             #:streaming? #f)))
    (match (response-code res)
      (200
       (json-string->scm (bytevector->string body "utf-8")))
      (_
       ;; Error
       (throw 'mastodon `(("response-code" . ,(response-code res))
                          ("response-phrase" .
                           ,(response-reason-phrase res))
                          ("response" .
                           ,(if (bytevector? body)
                                (bytevector->string body "utf-8")
                                body))))))))

;;;
;;; Request syntax.
;;;

(define-syntax-rule (define-get-request DOCSTRING (name instance args ...)
                      path path-suffix json->value)
  (define (name instance args ...)
    DOCSTRING
    (let ((url (string-append (mastodon-url instance)
                              path args ... path-suffix)))
      (json->value (mastodon-api-get url (mastodon-token instance))))))

;;;
;;; Accounts.
;;;

(define-get-request
  "Get account by user ID. Return account."
  (mtd-accounts-by-id instance id)
  "/api/v1/accounts/" ""
  json->account)

(define-get-request
  "Get current account. Return account."
  (mtd-accounts-verify-credentials instance)
  "/api/v1/accounts/verify_credentials" ""
  json->account)

(define-get-request
  "Get account followers. Return list of accounts."
  (mtd-accounts-id-followers instance id)
  "/api/v1/accounts/" "/followers"
  (λ (l) (map json->account l)))

(define (mtd-accounts-id-follow instance id)
  "Follow an account corresponding to ID. Return relationship."
  (let ((url (string-append (mastodon-url instance)
                            "/api/v1/accounts/" id "/follow")))
    (json->relationship (mastodon-api-post url "" (mastodon-token instance)))))

(define (mtd-accounts-id-unfollow instance id)
  "Unfollow an account corresponding to ID. Return relationship."
  (let ((url (string-append (mastodon-url instance)
                            "/api/v1/accounts/" id "/unfollow")))
    (json->relationship (mastodon-api-post url "" (mastodon-token instance)))))

(define-get-request
  "Get following accounts of account coresponding to ID. Return list of
accounts."
  (mtd-accounts-id-following instance id)
  "/api/v1/accounts/" "/following"
  (λ (l) (map json->account l)))

(define-get-request
  "Get statuses of account corresponding to ID. Return list of statuses."
  (mtd-accounts-id-statuses instance id)
  "/api/v1/accounts/" "/statuses"
  (λ (l) (map json->status l)))

(define-get-request
  "Search an accounts by username, domain and display name, corresponding to
NAME argument. Return list of accounts."
  (mtd-accounts-search instance name)
  "/api/v1/accounts/search?q=" ""
  (λ (l) (map json->account l)))

;;;
;;; Apps.
;;;

(define (mtd-apps-verify-credentials instance)
  "Confirm that the app’s OAuth2 credentials work. Return application."
  (let ((url (string-append (mastodon-url instance)
                            "/api/v1/apps/verify_credentials")))
    (json->application (mastodon-api-get url (mastodon-token instance)))))

;;;
;;; Blocks.
;;;

(define (mtd-account-blocked instance)
  "Accounts the user has blocked. Return list of account."
  (let ((url (string-append (mastodon-url instance)
                            "/api/v1/blocks")))
    (map json->account (mastodon-api-get url (mastodon-token instance)))))

(define (mtd-account-block instance id)
  "Block an account. Returns relationship"
  (let ((url (string-append (mastodon-url instance)
                            "/api/v1/accounts/" id "/block")))
    (json->relationship (mastodon-api-post url "" (mastodon-token instance)))))

(define (mtd-account-unblock instance id)
  "Unblock an account. Returns relationship"
  (let ((url (string-append (mastodon-url instance)
                            "/api/v1/accounts/" id "/unblock")))
    (json->relationship (mastodon-api-post url "" (mastodon-token instance)))))

;;;
;;; Emoji.
;;;

(define (mtd-custom-emojis instance)
  "Custom emojis that are available on the server. Return list of Emoji."
  (let ((url (string-append (mastodon-url instance)
                            "/api/v1/custom_emojis")))
    (map json->emoji (mastodon-api-get url (mastodon-token instance)))))

;;;
;;; Domain blocks.
;;;

(define (mtd-domain-blocked instance)
  "Domains the user has blocked. Return list of string."
  (let ((url (string-append (mastodon-url instance)
                            "/api/v1/domain_blocks")))
    (mastodon-api-get url (mastodon-token instance))))

(define (mtd-domain-block instance domain)
  "Block a DOMAIN to hide all public posts from it, all notifications from it,
and remove all followers from it."
  (let ((url (string-append (mastodon-url instance)
                            "/api/v1/domains/domain_blocks"))
        (data (scm->json-string `(("domain" . ,domain)))))
    (mastodon-api-post url data
                       (mastodon-token instance))))

(define (mtd-domain-unblock instance domain)
  "Remove a DOMAIN block."
  (let ((url (string-append (mastodon-url instance)
                            "/api/v1/domains/domain_blocks"))
        (data (scm->json-string `(("domain" . ,domain)))))
    (mastodon-api-delete url (mastodon-token instance)
                         #:data data
                         #:content-type "application/json")))

;;;
;;; Endorsements.
;;;

(define (mtd-endorsed instance)
  "List of Endorsements accounts. Return accounts list."
  (let ((url (string-append (mastodon-url instance)
                            "/api/v1/endorsements")))
    (map json->account (mastodon-api-get url (mastodon-token instance)))))

(define (mtd-endorse instance id)
  "Endorse an account, i.e. choose to feature the account on the user’s public
profile. Returns Relationship"
  (let ((url (string-append (mastodon-url instance)
                            "/api/v1/accounts/" id "/pin")))
    (json->relationship (mastodon-api-post url "" (mastodon-token instance)))))

(define (mtd-unendorse instance id)
  "Undo endorse of an account. Returns Relationship"
  (let ((url (string-append (mastodon-url instance)
                            "/api/v1/accounts/" id "/unpin")))
    (json->relationship (mastodon-api-post url "" (mastodon-token instance)))))

;;;
;;; Favourites.
;;;

(define (mtd-favourited instance)
  "Statuses the user has favourited. Return list of status."
  (let ((url (string-append (mastodon-url instance)
                            "/api/v1/favourites")))
    (map json->status (mastodon-api-get url (mastodon-token instance)))))

(define (mtd-favourite-status instance id)
  "Favourite a status. Return Status."
  (let ((url (string-append (mastodon-url instance)
                            "/api/v1/statuses/" id "/favourite")))
    (json->status (mastodon-api-post url "" (mastodon-token instance)))))

(define (mtd-unfavourite-status instance id)
  "Undo the favourite of a status. Return Status."
  (let ((url (string-append (mastodon-url instance)
                            "/api/v1/statuses/" id "/unfavourite")))
    (json->status (mastodon-api-post url "" (mastodon-token instance)))))

;;;
;;; Instance.
;;;

(define (mtd-instance-info instance)
  "Get information about the server. Returns instance."
  (let ((url (string-append (mastodon-url instance)
                            "/api/v1/instance")))
    (json->instance (mastodon-api-get url (mastodon-token instance)))))

;;;
;;; Statuses.
;;;

(define (mtd-status-by-id instance id)
  "Get status coresponding to ID. Return status."
  (let ((url (string-append (mastodon-url instance)
                            "/api/v1/statuses/" id)))
    (json->status (mastodon-api-get url (mastodon-token instance)))))

(define (mtd-status-context-by-id instance id)
  "Get status context coresponding to ID. Return context."
  (let ((url (string-append (mastodon-url instance)
                            "/api/v1/statuses/" id "/context")))
    (json->context (mastodon-api-get url (mastodon-token instance)))))

(define (mtd-status-card-by-id instance id)
  "Get status card coresponding to ID. Return card."
  (let ((url (string-append (mastodon-url instance)
                            "/api/v1/statuses/" id "/card")))
    (json->card (mastodon-api-get url (mastodon-token instance)))))

(define (mtd-status-id-reblogged-by instance id)
  "Get list of accounts who reblogged status coresponding to ID. Return list
of accounts."
  (let ((url (string-append (mastodon-url instance)
                            "/api/v1/statuses/" id "/reblogged_by")))
    (map json->account (mastodon-api-get url (mastodon-token instance)))))

(define (mtd-status-id-favourited-by instance id)
  "Get list of accounts who favourited status coresponding to ID. Return list
of accounts."
  (let ((url (string-append (mastodon-url instance)
                            "/api/v1/statuses/" id "/favourited_by")))
    (map json->account (mastodon-api-get url (mastodon-token instance)))))

(define (mtd-delete-status-by-id instance id)
  "Delete status coresponding to ID."
  (let ((url (string-append (mastodon-url instance)
                            "/api/v1/statuses/" id)))
    (mastodon-api-delete url (mastodon-token instance))))

(define (mtd-new-status instance args)
  "Post a new status on INSTANCE. Return status.

ARGS is list of parameters. You need to provide \"status\" and/or
\"media_ids\"."
  (let ((url  (string-append (mastodon-url instance)
                             "/api/v1/statuses"))
        (data (scm->json-string args)))
    (json->status (mastodon-api-post url data
                                     (mastodon-token instance)
                                     #:content-type "application/json"))))

(define (mtd-status-id-reblog instance id)
  "Reblog status coresponding to ID. Return status."
  (let ((url (string-append (mastodon-url instance)
                            "/api/v1/statuses/" id "/reblog")))
    (json->status (mastodon-api-post url "" (mastodon-token instance)))))

(define (mtd-status-id-unreblog instance id)
  "Unreblog status coresponding to ID. Return status."
  (let ((url (string-append (mastodon-url instance)
                            "/api/v1/statuses/" id "/unreblog")))
    (json->status (mastodon-api-post url "" (mastodon-token instance)))))

(define (mtd-status-id-pin instance id)
  "Pin status coresponding to ID. Return status."
  (let ((url (string-append (mastodon-url instance)
                            "/api/v1/statuses/" id "/pin")))
    (json->status (mastodon-api-post url "" (mastodon-token instance)))))

(define (mtd-status-id-unpin instance id)
  "Unpin status coresponding to ID. Return status."
  (let ((url (string-append (mastodon-url instance)
                            "/api/v1/statuses/" id "/unpin")))
    (json->status (mastodon-api-post url "" (mastodon-token instance)))))

;;;
;;; Search.
;;;

(define (mtd-search instance query)
  "Search for content in accounts, statuses and hashtags. Return results."
  (let ((url (string-append (mastodon-url instance)
                            "/api/v2/search?q=" query)))
    (json->results (mastodon-api-get url (mastodon-token instance)))))

;;;
;;; Media.
;;;

(define (mtd-post-media instance filepath description)
  "Post new media from FILEPATH."
  (define mime-type
    '(("jpeg" . "image/jpeg")
      ("jpg"  . "image/jpeg")
      ("png"  . "image/png")
      ("gif"  . "image/gif")))

  (define file-name
    (car (last-pair (split-and-decode-uri-path filepath))))

  (define file-type
    (let* ((extention (car (last-pair (string-split file-name #\.))))
           (type (assoc-ref mime-type extention)))
      (if (string? type)
          type
          (throw 'mastodon '(("error" . "Unknown mime-type"))))))

  (define (form-data-encode file-u8)
    (let* ((string->u8-list (λ (str)
                              (bytevector->u8-list
                               (string->bytevector str "utf-8")))))
      (append (string->u8-list "--AaB03x\r\n")
              (string->u8-list
               (string-append
                "Content-Disposition: form-data; name=\"file\";"
                "filename=\"" file-name "\"\r\n"
                "Content-Type: " file-type "\r\n\r\n"))
              file-u8
              (string->u8-list "\r\n--AaB03x--\r\n"))))

  (let* ((url (string-append (mastodon-url instance)
                             "/api/v1/media"))
         (file-port (open-file filepath "rb"))
         (file-u8   (bytevector->u8-list
                     (get-bytevector-all file-port)))
         (data      (form-data-encode file-u8)))
    (close file-port)
    (json->attachment
     (mastodon-api-post url (u8-list->bytevector data)
                        (mastodon-token instance)
                        #:content-type
                        (string-append "multipart/form-data; "
                                       "boundary=AaB03x")))))

;;;
;;; Mutes.
;;;

(define (mtd-muted instance)
  "Accounts the user has muted. Return list of account."
  (let ((url (string-append (mastodon-url instance)
                            "/api/v1/mutes")))
    (json->account (mastodon-api-get url (mastodon-token instance)))))

(define (mtd-mute instance id)
  "Mute an account. Return relationship."
  (let ((url (string-append (mastodon-url instance)
                            "/api/v1/accounts/" id "/mute")))
    (json->relationship
     (mastodon-api-post url "" (mastodon-token instance)))))

(define (mtd-unmute instance id)
  "Unmute an account. Return relationship."
  (let ((url (string-append (mastodon-url instance)
                            "/api/v1/accounts/" id "/unmute")))
    (json->relationship
     (mastodon-api-post url "" (mastodon-token instance)))))

(define (mtd-status-mute instance id)
  "Mute the conversation the status is part of, to no longer be notified about
it. Return status."
  (let ((url (string-append (mastodon-url instance)
                            "/api/v1/statuses/" id "/mute")))
    (json->status (mastodon-api-post url "" (mastodon-token instance)))))

(define (mtd-status-unmute instance id)
  "Unmute the conversation the status is part of. Return status."
  (let ((url (string-append (mastodon-url instance)
                            "/api/v1/statuses/" id "/unmute")))
    (json->status (mastodon-api-post url "" (mastodon-token instance)))))
