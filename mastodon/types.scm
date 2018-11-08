;;; types.scm --- Guile module for mastodon.
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

(define-module (mastodon types)
  #:use-module (srfi srfi-9)
  #:export (<mastodon-instance>
            instance
            instance?
            instance-name
            instance-url
            instance-token

            <mastodon-account>
            account
            account?
            account-id
            account-username
            account-acct
            account-display-name
            account-locked
            account-created-at
            account-followers-counts
            account-following-counts
            account-statuses-counts
            account-note
            account-url
            account-avatar
            account-avatar-static
            account-header
            account-header-static
            account-emojis
            account-moved
            account-fields
            account-bot

            <mastodon-field>
            field?
            field-name
            field-value
            field-verified-at))

;;;
;;; Define types
;;;

;;; Instance
(define-record-type <mastodon-instance>
  (instance name url token)
  instance?
  (name  instance-name)
  (url   instance-url)
  (token instance-token))

;;; Account
(define-record-type <mastodon-account>
  (account id             username acct   display-name locked
           created-at     followers-count following-count
           statuses-count note            url
           avatar         avatar-static   header
           header-static  emojis          moved
           fields         bot)
  account?
  ;; source: https://docs.joinmastodon.org/api/entities/#account
  (id              account-id)               ;String
  (username        account-username)         ;String
  (acct            account-acct)             ;String
  (display-name    account-display-name)     ;String
  (locked          account-locked)           ;Boolean
  (created-at      account-created-at)       ;String (Datetime)
  (followers-count account-followers-counts) ;Number
  (following-count account-following-counts) ;Number
  (statuses-count  account-statuses-counts)  ;Number
  (note            account-note)             ;String
  (url             account-url)              ;String (URL)
  (avatar          account-avatar)           ;String (URL)
  (avatar-static   account-avatar-static)    ;String (URL)
  (header          account-header)           ;String (URL)
  (header-static   account-header-static)    ;String (URL)
  (emojis          account-emojis)           ;List of Emoji
  (moved           account-moved)            ;Account
  (fields          account-fields)           ;List of Hash
  (bot             account-bot))             ;Boolean

;;; Field
(define-record-type <mastodon-field>
  (field name value verified-at)
  field?
  (name        field-name)              ;String
  (value       field-value)             ;String (HTML)
  (verified-at field-verified-at))      ;String (Datetime)
