# XMonad configuration #

Xmonad is a fully programmable tiling window manager in haskell.
This is my variant of XMonad.

It features topic spaces, which are controllable with a small home grown stack based language.
It manages several tmux instances associated with the topic spaces.
It works together with my dotfiles config.

# Installation #

This is almost undoable for others, I just shared it for sharing some ideas. But for the willing:

import my dotfiles repository.
Copy all files to your home directory.

install rxvt(d), tmux, zsh, fbpanel, trayer, diodon and quiterss

Change .xmonad.zsh if you want.
Change tmux.sh if you want.

compile with:

    cabal configure
	cabal build

cp dist/build/xmonad/xmonad xmonad

and run xmonad in an XSession.
