# xmonad Configuration

This repository contains my [xmonad] [] configuration.  I thought I'd
share it with the rest of the world because I'm doing a few
interesting things:

  1. I use either [stack][] or Cabal to build xmonad and compile my
     configuration into an executable (`xmonadrc`).  This allows me to
     easily use my custom `xmonad` and `xmonad-contrib` sources (in
     `vendor`).

     Thanks to changes in `xmonad` version 0.13, It's really easy for
     me to use my `xmonadrc` executable in my `~/.xinitrc` instead of
     starting `xmonad` directly.

     Note: I actually compile everything
     using [my custom nixpkgs script] [xmonadrc.nix].  This brings in
     all the dependencies needed to build everything in this repo.

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
[xmonadrc.nix]: https://github.com/pjones/nix-utils/blob/master/pkgs/haskell/xmonadrc.nix
