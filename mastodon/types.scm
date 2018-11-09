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
            field-verified-at

            <mastodon-source>
            source?
            source-privacy
            source-sensitive
            source-language
            source-note
            source-fields

            <mastodon-application>
            application?
            application-name
            application-website

            <mastodon-attachment>
            attachment-id
            attachment-type
            attachment-url
            attachment-remote-url
            attachment-preview-url
            attachment-text-url
            attachment-meta
            attachment-description

            <mastodon-emoji>
            emoji-shortcode
            emoji-static-url
            emoji-url
            emoji-visible-in-picker

            ;; Parser
            hashtab->account))

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

;;; Source
(define-record-type <mastodon-source>
  (source privacy sensitive language note fields)
  source?
  (privacy   source-privacy)            ;String
  (sensitive source-sensitive)          ;Boolean
  (language  source-language)           ;String (ISO6391)
  (note      source-note)               ;String
  (fields    source-fields))            ;List of Hash

;;; Application
(define-record-type <mastodon-application>
  (application name website)
  application?
  (name    application-name)            ;String
  (website application-website))        ;String (URL)

;;; Attachment
(define-record-type <mastodon-attachment>
  (attachment id type url remote-url preview-url
              text-url meta description)
  attachment?
  (id           attachment-id)           ;String
  (type         attachment-type)         ;String (Enum)
  (url          attachment-url)          ;String (URL)
  (remote-url   attachment-remote-url)   ;String (URL)
  (preview-url  attachment-preview-url)  ;String (URL)
  (text-url     attachment-text-url)     ;String (URL)
  (meta         attachment-meta)         ;Hash
  (description  attachment-description)) ;String

;;; Emoji
(define-record-type <mastodon-emoji>
  (emoji shortcode static-url url visible-in-picker)
  emoji?
  (shortcode         emoji-shortcode)          ;String
  (static-url        emoji-static-url)         ;String (URL)
  (url               emoji-url)                ;String (URL)
  (visible-in-picker emoji-visible-in-picker)) ;Boolean

;;;
;;; Parser
;;;

(define (hashtab->account ht)
  "Return account record type from json hash-tab."
  (account (hash-ref ht "id")
           (hash-ref ht "username")
           (hash-ref ht "acct")
           (hash-ref ht "display_name")
           (hash-ref ht "locked")
           (hash-ref ht "created_at")
           (hash-ref ht "followers_counts")
           (hash-ref ht "following_counts")
           (hash-ref ht "statuses_counts")
           (hash-ref ht "note")
           (hash-ref ht "url")
           (hash-ref ht "avatar")
           (hash-ref ht "avatar_static")
           (hash-ref ht "header")
           (hash-ref ht "header_static")
           (hash-ref ht "emojis")
           (hash-ref ht "moved")
           (hash-ref ht "fields")
           (hash-ref ht "bot")))
