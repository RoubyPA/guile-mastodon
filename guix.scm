;; guix.scm --- Guile-mastodon guix package.
;; Copyright (C) 2018  Pierre-Antoine Rouby
;;
;; This program is free software: you can redistribute it and/or modify
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(use-modules ((guix licenses) #:prefix license:)
             (guix packages)
             (guix download)
             (guix git-download)
             (guix gexp)
             (guix utils)
             (guix build-system gnu)
             (gnu packages)
             (gnu packages tls)
             (gnu packages guile)
             (gnu packages autotools)
             (gnu packages pkg-config))

(package
  (name "guile-mastodon")
  (version "0.0.1")
  (source (local-file "." "guile-mastodon"
                      #:recursive? #t))
  (build-system gnu-build-system)
  (native-inputs
   `(("autoconf" ,autoconf)
     ("automake" ,automake)
     ("pkg-config" ,pkg-config)))
  (propagated-inputs
   `(("guile" ,guile-2.2)
     ("guile-json" ,guile-json)
     ("gnutls" ,gnutls)))
  (home-page "https://framagit.org/prouby/guile-mastodon")
  (synopsis "Guile mastodon module.")
  (description "Mastodon module for guile.")
  (license license:gpl3+))
