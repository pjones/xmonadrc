# xmonad Configuration

This repository contains my [xmonad][] configuration.

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

### Tree Column Layout (My Primary Layout)

Master window is in the center.  All other windows automatically
balance between the two side columns.

<div>
  <a href="https://www.pmade.com/static/images/2019/d07b30f392c60e8a145f1667f482c73c.png">
    <img style="max-width: 400px" src="https://www.pmade.com/static/images/2019/d07b30f392c60e8a145f1667f482c73c.png"/>
  </a>
</div>

### Grid Layout (For Chat Windows)

A 3x2 grid.

<div>
  <a href="https://www.pmade.com/static/images/2019/19aa430790f325de5d7d43e7ad701cfc.png">
    <img style="max-width: 400px" src="https://www.pmade.com/static/images/2019/19aa430790f325de5d7d43e7ad701cfc.png"/>
  </a>
</div>

### Focus Layout

Focuses in on windows tagged "focus".  Other windows are moved up into
a top bar.

<div>
  <a href="https://www.pmade.com/static/images/2019/cc4d7480c69008cdadc8f419cb987f8f.png">
    <img style="max-width: 400px" src="https://www.pmade.com/static/images/2019/cc4d7480c69008cdadc8f419cb987f8f.png"/>
  </a>
</div>


[xmonad]: http://xmonad.org/
