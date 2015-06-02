# My Emacs config

This is an Emacs 24 setup based on [Cask](https://github.com/cask/cask).

## Bootstrap

	brew install cask
	cd ~/.emacs.d
	cask install

For packages loaded from a local working copy instead of their elpa
release, link them into cask; this is necessary after a major upgrade
of emacs (e.g. 24.4 to 24.5):

    cask link vitamined-mode-line ./lisp-devel/vitamined-mode-line

After these steps, everything should be OK.
