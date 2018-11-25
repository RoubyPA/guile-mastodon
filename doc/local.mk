if EMACS
   EMACS = emacs -Q --batch
endif

ORG2INFO = -f org-mode -f org-texinfo-export-to-texinfo

BUILT_SOURCES =					\
  doc/guile-mastodon.info

$(srcdir)/%D%/guile-mastodon.info: $(srcdir)/%D%/guile-mastodon.org
	$(EMACS) --file=$(srcdir)/%D%/guile-mastodon.org $(ORG2INFO)
	$(MAKEINFO) $(srcdir)/%D%/guile-mastodon.texi

install-data-hook:
	mkdir -p $(infodir)
	install $(srcdir)/guile-mastodon.info $(infodir)/guile-mastodon.info
