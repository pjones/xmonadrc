# XMonad Configuration

This repository contains my [XMonad] [] configuration.  I thought I'd
share it with the rest of the world because I'm doing a few
interesting things:

  1. I use `cabal` sandboxes to build XMonad and compile my
     configuration into an executable (`xmonadrc`).  This allows me to
     easily use my custom xmonad and xmonad-contrib sources (in
     `vendor`).

     This presents an interesting challenge since XMonad by default
     tries to compile your `~/.xmonad/xmonad.hs` file when it starts.
     Therefore I install my generated `xmonadrc` binary where XMonad
     expects it and use `.cabal-sandbox/bin/xmonad` to restart the
     running XMonad.  I've also updated my `~/.xinitrc` to exec my
     `xmonadrc` directly without using the `xmonad` binary.

     All the details for this process are in the `scripts/install.sh`
     script.

  2. The `checkrc` executable that is built with `cabal` boots a mini
     XMonad environment in order to test my configuration before
     restarting.  Right now it's only testing the key bindings.

  3. The majority of my key bindings are underneath a prefix key:
     `C-z`.  Being a long time user of tools like GNU screen, tmux,
     and Emacs, I've become very accustomed to prefix keys.  I quite
     like them.

## Source Code Organization

  * `xmonadrc.hs`: This is where the `main` function lives.

  * `src/XMonad/Local/Action.hs`: Event handling.  I have an
    interesting function in there called `focusFollowsTiledOnly` that
    enables focus-follows-mouse in the tiled layer but not in the
    floating layer.  I found that:

        focus-follows-mouse + update-pointer + floating-windows = weird-stuff

  * `src/XMonad/Local/Keys.hs`: All of my key bindings, grouped by
    functionality.

  * `src/XMonad/Local/Layout.hs`: Layout rules.

  * `src/XMonad/Local/Log.hs`: Log hook,

  * `src/XMonad/Local/Music.hs`: Functions to switch radio stations
    and select albums to play in MPD.

  * `src/XMonad/Local/Prompt.hs`: XPrompt configuration.

  * `src/XMonad/Local/Workspaces.hs`: The names of my workspaces plus
     project configuration for `XMonad.Actions.DynamicProjects`.

## Using this Configuration

If you are interested in using `cabal` to build your XMonad
configuration then you might also be interested in the following
outline of the steps I take to build, install, and restart XMonad.

  1. Install the [Haskell Platform] [].

  2. Install `cabal-install >= 0.18`:

        $ cabal update
        $ cabal install cabal-install

  3. Use the `scrips/build.sh` and `xmonadrc.cabal` files in this
     directory to build your XMonad configuration.

        $ scripts/build.sh

  4. Install and restart XMonad (assumes XMonad is currently running):

        $ scripts/install.sh restart

## Screenshots

<div>
  <a href="http://www.pmade.com/static/images/2014/8f30165dfb35240966263058bc8f752e.png">
    <img style="max-width: 400px" src="http://www.pmade.com/static/images/2014/8f30165dfb35240966263058bc8f752e.png"/>
  </a>
</div>


[xmonad]: http://xmonad.org/
[haskell platform]: http://www.haskell.org/platform/
