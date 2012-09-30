.PHONY: all

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
	# nothing to do

clean:
	rm -f $(emacs-backups)
