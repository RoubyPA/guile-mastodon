;;; types.scm --- Guile module for mastodon.
;;
;; Copyright (C) 2018 by Pierre-Antoine Rouby <contact@parouby.fr>
;; Copyright (C) 2018 by Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (ice-9 match)
  #:use-module (json)
  #:export (<mastodon-mastodon>
            make-mastodon
            mastodon?
            mastodon-name
            mastodon-url
            mastodon-token

            <mastodon-account>
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
            attachment?
            attachment-id
            attachment-type
            attachment-url
            attachment-remote-url
            attachment-preview-url
            attachment-text-url
            attachment-meta
            attachment-description

            <mastodon-emoji>
            emoji?
            emoji-shortcode
            emoji-static-url
            emoji-url
            emoji-visible-in-picker

            <mastodon-status>
            status?
            status-id
            status-uri
            status-url
            status-account
            status-in-reply-to-id
            status-in-reply-to-account-id
            status-reblog
            status-content
            status-created-at
            status-emojis
            status-replies-count
            status-reblogs-count
            status-favourites-count
            status-reblogged
            status-favourited
            status-muted
            status-sensitive
            status-spoiler-text
            status-visibility
            status-media-attachments
            status-mentions
            status-tags
            status-card
            status-application
            status-language
            status-pinned

            <mastodon-relationship>
            relationship?
            relationship-id
            relationship-following
            relationship-followed-by
            relationship-blocking
            relationship-muting
            relationship-muting-notifications
            relationship-requested
            relationship-domain-blocking
            relationship-showing-reblogs
            relationship-endorsed

            <mastodon-instance>
            instance?
            instance-uri
            instance-title
            instance-description
            instance-email
            instance-version
            instance-thumbnail
            instance-urls
            instance-stats
            instance-languages
            instance-contact-account

            <mastodon-context>
            context?
            context-ancestors
            context-descendants

            <mastodon-card>
            card?
            card-url
            card-title
            card-description
            card-image
            card-type
            card-author-name
            card-author-url
            card-provider-name
            card-provider-url
            card-html
            card-width
            card-height

            <mastodon-results>
            results?
            results-accounts
            results-statuses
            results-hashtags

            ;; Parser
            json->account
            json->field
            json->source
            json->application
            json->attachment
            json->emoji
            json->status
            json->relationship
            json->instance
            json->context
            json->card
            json->results))

(define-syntax-rule (define-json-reader json->record ctor spec ...)
  "Define JSON->RECORD as a procedure that converts a JSON representation,
read from a port, string, or hash table, into a record created by CTOR and
following SPEC, a series of field specifications."
  (define (json->record input)
    (let ((table (cond ((port? input)
                        (json->scm input))
                       ((string? input)
                        (json-string->scm input))
                       ((hash-table? input)
                        input))))
      (let-syntax ((extract-field (syntax-rules ()
                                    ((_ table (field key json->value))
                                     (json->value (hash-ref table key)))
                                    ((_ table (field key))
                                     (hash-ref table key))
                                    ((_ table (field))
                                     (hash-ref table
                                               (symbol->string 'field))))))
        (ctor (extract-field table spec) ...)))))

(define-syntax-rule (define-json-mapping rtd ctor pred json->record
                      (field getter spec ...) ...)
  "Define RTD as a record type with the given FIELDs and GETTERs, à la SRFI-9,
and define JSON->RECORD as a conversion from JSON to a record of this type."
  (begin
    (define-record-type rtd
      (ctor field ...)
      pred
      (field getter) ...)

    (define-json-reader json->record ctor
      (field spec ...) ...)))

;;;
;;; Define record types
;;;

;;; Instance mastodon
(define-record-type <mastodon-mastodon>
  (make-mastodon name url token)
  mastodon?
  (name  mastodon-name)
  (url   mastodon-url)
  (token mastodon-token))

;;;
;;; JSON Mapping
;;;

;;; Account <https://docs.joinmastodon.org/api/entities/#account>
(define-json-mapping <mastodon-account>
  make-account
  account?
  json->account
  (id              account-id)
  (username        account-username)
  (acct            account-acct)
  (display-name    account-display-name "display_name")
  (locked          account-locked)
  (created-at      account-created-at "created_at")
  (followers-count account-followers-count "followers_count")
  (following-count account-following-count "following_count")
  (statuses-count  account-statuses-count "statuses_count")
  (note            account-note)
  (url             account-url)
  (avatar          account-avatar)
  (avatar-static   account-avatar-static "avatar_static")
  (header          account-header)
  (header-static   account-header-static "header_static")
  (emojis          account-emojis)
  (moved           account-moved)
  (fields          account-fields)
  (bot             account-bot))

;;; Field <https://docs.joinmastodon.org/api/entities/#field>
(define-json-mapping <mastodon-field>
  make-field
  field?
  json->field
  (name        field-name)
  (value       field-value)
  (verified-at field-verified-at "verified_at"))

;;; Source <https://docs.joinmastodon.org/api/entities/#source>
(define-json-mapping <mastodon-source>
  make-source
  source?
  json->source
  (privacy   source-privacy)
  (sensitive source-sensitive)
  (language  source-language)
  (note      source-note)
  (fields    source-fields))

;;; Application <https://docs.joinmastodon.org/api/entities/#application>
(define-json-mapping <mastodon-application>
  make-application
  application?
  json->application
  (name    application-name)
  (website application-website))

;;; Attachment <https://docs.joinmastodon.org/api/entities/#attachment>
(define-json-mapping <mastodon-attachment>
  make-attachment
  attachment?
  json->attachment
  (id           attachment-id)
  (type         attachment-type)
  (url          attachment-url)
  (remote-url   attachment-remote-url "remote_url")
  (preview-url  attachment-preview-url "preview_url")
  (text-url     attachment-text-url "text_url")
  (meta         attachment-meta)
  (description  attachment-description))

;;; Emoji <https://docs.joinmastodon.org/api/entities/#emoji>
(define-json-mapping <mastodon-emoji>
  make-emoji
  emoji?
  json->emoji
  (shortcode         emoji-shortcode)
  (static-url        emoji-static-url "static_url")
  (url               emoji-url)
  (visible-in-picker emoji-visible-in-picker "visible_in_picker"))

;;; Status <https://docs.joinmastodon.org/api/entities/#status>
(define-json-mapping <mastodon-status>
  make-status
  status?
  json->status
  (id                     status-id)
  (uri                    status-uri)
  (url                    status-url)
  (account                status-account)
  (in-reply-to-id         status-in-reply-to-id "in_reply_to_id")
  (in-reply-to-account-id status-in-reply-to-account-id "in_reply_to_account_id")
  (reblog                 status-reblog)
  (content                status-content)
  (created-at             status-created-at "created_at")
  (emojis                 status-emojis)
  (replies-count          status-replies-count "replies_count")
  (reblogs-count          status-reblogs-count "reblogs_count")
  (favourites-count       status-favourites-count "favourites_count")
  (reblogged              status-reblogged)
  (favourited             status-favourited)
  (muted                  status-muted)
  (sensitive              status-sensitive)
  (spoiler-text           status-spoiler-text "spoiler_text")
  (visibility             status-visibility)
  (media-attachments      status-media-attachments "media_attachments")
  (mentions               status-mentions)
  (tags                   status-tags)
  (card                   status-card)
  (application            status-application)
  (language               status-language)
  (pinned                 status-pinned))

;;; Relationship <https://docs.joinmastodon.org/api/entities/#relationship>
(define-json-mapping <mastodon-relationship>
  make-relationship
  relationship?
  json->relationship
  (id                   relationship-id)
  (following            relationship-following)
  (followed-by          relationship-followed-by "followed_by")
  (blocking             relationship-blocking)
  (muting               relationship-muting)
  (muting-notifications relationship-muting-notifications "muting_notifications")
  (requested            relationship-requested)
  (domain-blocking      relationship-domain-blocking "domain_blocking")
  (showing-reblogs      relationship-showing-reblogs "showing_reblogs")
  (endorsed             relationship-endorsed))

;;; Instance <https://docs.joinmastodon.org/api/entities/#instance>
(define-json-mapping <mastodon-instance>
  make-instance
  instance?
  json->instance
  (uri             instance-uri)
  (title           instance-title)
  (description     instance-description)
  (email           instance-email)
  (version         instance-version)
  (thumbnail       instance-thumbnail)
  (urls            instance-urls)
  (stats           instance-stats)
  (languages       instance-languages)
  (contact-account instance-contact-account "contact_account"))

;;; Context <https://docs.joinmastodon.org/api/entities/#context>
(define-json-mapping <mastodon-context>
  make-context
  context?
  json->context
  (ancestors   context-ancestors)
  (descendants context-descendants))

;;; Card <https://docs.joinmastodon.org/api/entities/#card>
(define-json-mapping <mastodon-card>
  make-card
  card?
  json->card
  (url           card-url)
  (title         card-title)
  (description   card-description)
  (image         card-image)
  (type          card-type)
  (author-name   card-author-name "author_name")
  (author-url    card-author-url "author_url")
  (provider-name card-provider-name "provider_name")
  (provider-url  card-provider-url "provider_url")
  (html          card-html)
  (width         card-width)
  (height        card-height))

;;; Results <https://docs.joinmastodon.org/api/entities/#results>
(define-json-mapping <mastodon-results>
  make-results
  results?
  json->results
  (accounts results-accounts)
  (statuses results-statuses)
  (hashtags results-hashtags))
