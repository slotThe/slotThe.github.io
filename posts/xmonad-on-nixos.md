---
title: Using XMonad with NixOS
date: 2023-11-13
tags: emacs, nix, xmonad
---

<p>
I recently switched to NixOS,
and one of the first tasks was to properly set up my window manager of choice—XMonad, of course.
Luckily, the project provides a custom flake that makes pretty very straightforward;
if you know your way around flakes and Nix, that is.
I don't yet, so I hit some rough spots.
Since providing more documentation always sounds like a worthwhile goal,
this post is just that: a diff-by-diff guide on how everything was set up on my side.
</p>

<!--more-->

As indicated, I'll be working with direct diffs from my configuration;
this seems to tell the story in the most accurate way.
Because scrolling through lots of diffs is about as fun as a visit to the dentist,
they are all tucked away in expandable sections
<details>
  <summary>Like this one</summary>
  Hi :)
</details>

# Setup

First and foremost I should mention that XMonad does in fact have some documentation on how to set up the flake:
it's tucked away in [NIX.md](https://github.com/xmonad/xmonad-contrib/blob/master/NIX.md).
The process mainly consists of two parts:
adding an `xmonad-contrib` input to your system flake,
and enabling this functionality in `windowManager.xmonad`.

<details>
  <summary>Modifying the system flake</summary>
``` diff
diff --git a/flake.nix b/flake.nix
index 4cf35e4..dedd839 100644
--- a/flake.nix
+++ b/flake.nix
@@ -6,13 +6,14 @@
     emacs-overlay.url = github:nix-community/emacs-overlay;
     hmenu.url         = gitlab:slotThe/hmenu;
     kmonad.url        = git+https://github.com/kmonad/kmonad?submodules=1&dir=nix;
+    xmonad-contrib.url= github:xmonad/xmonad-contrib;
     home-manager      = {
       url = github:nix-community/home-manager;
       inputs.nixpkgs.follows = "nixpkgs";
     };
   };

-  outputs = inputs@{ self, nixpkgs, emacs-overlay, hmenu, kmonad, home-manager, ... }:
+  outputs = inputs@{ self, nixpkgs, emacs-overlay, hmenu, kmonad, xmonad-contrib, home-manager, ... }:
     let my-overlays = {
           nixpkgs.overlays = [
             emacs-overlay.overlays.default
@@ -20,7 +21,7 @@
           ];
         };
     in {
-      nixosConfigurations.comonad = nixpkgs.lib.nixosSystem {
+      nixosConfigurations.comonad = nixpkgs.lib.nixosSystem rec {
         system  = "x86_64-linux";
         modules = [
           ./nix/hardware-configuration.nix
@@ -34,6 +35,8 @@
             home-manager.useUserPackages = true;
             home-manager.users.slot      = import ./nix/home.nix;
           }
+        ] ++ xmonad-contrib.nixosModules ++ [
+          xmonad-contrib.modernise.${system}
         ];
       };
     };
```
</details>

After having added the input, making use of the flake is quite easy.[^2]

<details>
  <summary>Enabling the flake</summary>
``` diff
diff --git a/nix/configuration.nix b/nix/configuration.nix
index 4700e14..50b1519 100644
--- a/nix/configuration.nix
+++ b/nix/configuration.nix
@@ -63,6 +63,16 @@
     enable = true;
     layout = "us";
     displayManager.startx.enable = true;
+    windowManager.xmonad = {
+      enable = true;
+      enableContribAndExtras = true;
+      flake  = {
+        enable   = true;
+        compiler = "ghc947";
+      };
+      config = builtins.readFile ../xmonad/xmonad.hs;
+      enableConfiguredRecompile = true;
+    };
   };

   fonts.packages = with pkgs; [
```
</details>

And that's really it!
No need to write a custom `launch` function and similar shenanigans,
the flake takes care of all of those details for you—pretty neat, I think;
thanks [Leary](https://github.com/lsleary)!

# Primitive Emacs integration

Though not directly relevant to making the flake work,
a nice-to-have feature is Emacs support.
One could [create a cabal project][nix:xmonad-hs-cabal-project] to get real `haskell-language-server` support,
but that seems a bit overkill to me.
I don't really need more than a `ghci` session for my configuration<!--
-->—anything complicated enough to warrant a language server should immediately be upstreamed into `xmonad-contrib`.

In Emacs's `haskell-mode`,
executing `haskell-interactive-bring` (bound to `C-c C-c` by default)
brings up a `ghci` session.[^1]
Rather obviously, this fails out of the box.
It tries to call the global `ghci`,
which doesn't come from the flake,
and is thus not equipped with `xmonad-contrib` and other dependencies that I might have.

The low-tech solution I chose here is twofold;
first, I outsourced starting the Emacs daemon from my xinitrc to my XMonad configuration file.
In this way, the resulting Emacs instance comes equipped with the `$XMONAD_GHC` environment variable.

<details>
  <summary>Moving Emacs</summary>
``` diff
diff --git a/nix/modules/emacs.nix b/nix/modules/emacs.nix
index 794f636..fad9cd0 100644
--- a/nix/modules/emacs.nix
+++ b/nix/modules/emacs.nix
@@ -1,8 +1,6 @@
 { config, pkgs, ... }:

 {
-  services.emacs.enable = true; # Start as daemon
-
   xdg.configFile."emacs".source = config.lib.my.mkSymlink "emacs";

   home.packages = with pkgs; [
diff --git a/xinitrc/.xinitrc b/xinitrc/.xinitrc
index c9a6958..b8176bd 100755
--- a/xinitrc/.xinitrc
+++ b/xinitrc/.xinitrc
@@ -32,10 +32,6 @@ kmonad ~/.config/kmonad/config.kbd &
 # 14dec2019
 redshift -l 55.7:12.6 -t 6500K:3200K -b 1.0:0.8 &

-# Start emacs as a daemon.
-emacs --daemon &
-emacs --daemon=eshell &
-
 # Hide mouse cursor when idle.
 unclutter --ignore-scrolling &

@@ -46,4 +42,4 @@ wallpaper-changer &
 compton --blur-method kawase --blur-strength 5 --config ~/.config/compton/compton.conf &

 # Start the window manager.
-exec "$XMONAD_CONFIG_DIR"/xmonad-x86_64-linux
+exec xmonad
diff --git a/xmonad/xmonad.hs b/xmonad/xmonad.hs
index 5802888..933a274 100644
--- a/xmonad/xmonad.hs
+++ b/xmonad/xmonad.hs
@@ -80,6 +80,7 @@ import XMonad.Prompt.Workspace (workspacePrompt)

 import XMonad.Util.Cursor (setDefaultCursor)
 import XMonad.Util.EZConfig (additionalKeysP)
+import XMonad.Util.SpawnOnce (spawnOnce)
 import XMonad.Util.Loggers (logTitlesOnScreen)
 import XMonad.Util.NamedScratchpad (NamedScratchpad (NS), customFloating, namedScratchpadAction, namedScratchpadManageHook, scratchpadWorkspaceTag)
 import XMonad.Util.Run (EmacsLib (ElpaLib, Special), asBatch, asString, elispFun, eval, execute, executeNoQuote, findFile, getInput, inEditor, inEmacs, inProgram, inTerm, proc, progn, quote, require, setFrameName, setXClass, spawnExternalProcess, termInDir, toInput, withEmacsLibs, (>&&>), (>-$), (>->))
@@ -121,7 +122,10 @@ myConfig = def
   , normalBorderColor  = colorBg
   , focusedBorderColor = colorBlue
   , terminal           = "alacritty"
-  , startupHook        = setDefaultCursor xC_left_ptr
+  , startupHook        = do
+      setDefaultCursor xC_left_ptr
+      spawnOnce "emacs --daemon"        -- See Note [Emacs]
+      spawnOnce "emacs --daemon=eshell"
   , workspaces         = topicNames topics
   , manageHook         = myManageHook
   , layoutHook         = lessBorders (Combine Union Screen OnlyFloat) layoutOrder
@@ -132,6 +136,14 @@ myConfig = def
                          -- move pointer to exact center of that window.
   }

+{- Note [Emacs]
+
+All of the Emacs daemons are spawned here, and *not* as systemd services
+or in the ~/.xinitrc. The reason is that, in this way, Emacs inherits
+all environment variables that are set by the XMonad executable, like
+$XMONAD_GHC. See the bottom of this file for a use-case.
+-}
+
 -- | Building my own pretty-printer.
 xmobarPP :: ScreenId -> X PP
 xmobarPP sid = pure . filterOutWsPP [scratchpadWorkspaceTag] $ def
```
</details>

All that's left now is to set some local variable,
making Emacs aware that it should use another GHC version for my `xmonad.hs`.

<details>
<summary>Adding `haskell-process-path` to my `xmonad.hs`</summary>
``` diff
diff --git a/xmonad/xmonad.hs b/xmonad/xmonad.hs
index 933a274..185ed2e 100644
--- a/xmonad/xmonad.hs
+++ b/xmonad/xmonad.hs
@@ -881,3 +881,8 @@ switchToLayout = sendMessage . JumpToLayout
 -- submap from a list of @(key, action)@ pairs.
 basicSubmapFromList :: Ord key => [(key, action)] -> Map (KeyMask, key) action
 basicSubmapFromList = fromList . map \(k, a) -> ((0, k), a)
+
+--- Local Variables:
+--- mode: haskell
+--- eval: (setopt haskell-process-path-ghci (concat (or (getenv "XMONAD_GHC") "ghc") "i"))
+--- End:
```
</details>

Now `C-c C-c` works out of the box!

[nix:xmonad-hs-cabal-project]: https://srid.ca/xmonad-conf-ide

[^1]: More precisely, I have `C-c C-c` bound to

      ``` emacs-lisp
      (defun slot/haskell-load-and-bring ()
        "Sane behaviour when loading the current file into ghci."
        (interactive)
        (save-buffer)
        (haskell-process-load-file)
        (haskell-interactive-bring))
      ```

      but this is only a small quality of life wrapper.

[^2]: Note the presence of the `enableConfiguredRecompile` flag—this is necessary for `M-q` recompilation to work out of the box!
      I figured that out so you don't have to.
      Make sure that the revision of `nixpkgs` that you track is recent enough,
      else this flag may not be available.

      Also, don't forget to provide XMonad with your configuration,
      lest you will be greeted by a black screen upon login.
