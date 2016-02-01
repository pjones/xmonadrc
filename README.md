# xmonad Configuration

This repository contains my [xmonad] [] configuration.  I thought I'd
share it with the rest of the world because I'm doing a few
interesting things:

  1. I use [stack][] to build xmonad and compile my configuration into
     an executable (`xmonadrc`).  This allows me to easily use my
     custom `xmonad` and `xmonad-contrib` sources (in `vendor`).

     This presents an interesting challenge since xmonad by default
     tries to compile your `~/.xmonad/xmonad.hs` file when it starts.
     Therefore I install my generated `xmonadrc` binary where xmonad
     expects it and use it to restart the running xmonad instance.

  2. The `checkrc` executable that is built with [stack][] and boots a
     mini xmonad environment in order to test my configuration before
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

If you are interested in using `stack` to build your xmonad
configuration then you might also be interested in the following
outline of the steps I take to build, install, and restart xmonad.

  1. Install [stack][]

  3. Use [stack][] via the makefile to build the xmonad configuration:

        $ make

  4. Install and restart xmonad (assumes xmonad is currently running):

        $ make restart

If you use [Nixpkgs][] or [NixOS][] you may be interested in the
`build/nixpkgs.nix` file which is used by [stack][] to install
non-Haskell dependencies.

## Screenshots

<div>
  <a href="http://www.pmade.com/static/images/2014/8f30165dfb35240966263058bc8f752e.png">
    <img style="max-width: 400px" src="http://www.pmade.com/static/images/2014/8f30165dfb35240966263058bc8f752e.png"/>
  </a>
</div>


[xmonad]: http://xmonad.org/
[stack]: http://docs.haskellstack.org/en/stable/README.html
[nixpkgs]: http://nixos.org/nixpkgs/
[nixos]: http://nixos.org/
