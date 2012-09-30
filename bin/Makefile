.PHONY: all clean check

shell-scripts = check-itemiziation \
                check-newparser \
                check-num-xmls \
                check-text-dir-exists \
                exec-in-dir \
                fix-by-and-from \
                itemize \
                items-needed-for-item \
                listvoc \
                position-of-broken-in-mmllar \
                property-statistics \
                regen-html \
                timed-quiet-verify \
                verify-itemization \
                verify-quietly
shs = $(addsuffix .sh,$(shell-scripts))
emacs-backups = $(addsuffix ~,$(shs))


all:
	ok="1"; \
        for script in $(shs); do bash -n $$script || ok="0"; done; \
        [ "$$ok" = "1" ];

clean:
	rm -f $(emacs-backups)

check: all
