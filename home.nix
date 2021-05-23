{ config, pkgs, ... }:
let
  unstable = import <nixpkgs-unstable> { config = { allowUnfree = true; }; };
  RStudio-with-my-packages = pkgs.rstudioWrapper.override {
    packages = with pkgs.rPackages; [
      tidyverse
      knitr
      #reticulate
      xaringan
      kableExtra
      rticles
    ];
  };
  ghc-with-xmonad = pkgs.haskell.packages.ghc884.ghcWithPackages
    (ps: with ps; [ xmonad xmonad-contrib xmonad-extras ]);
in {
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "rucy";
  home.homeDirectory = "/home/rucy";

  programs.git = {
    enable = true;
    userName = "brurucy";
    userEmail = "brurucy@gmail.com";
  };
  # WM related stuff

  programs.fish = {
    enable = true;
    shellAliases = {
      ls = "exa";
      grep = "rg";
      cat = "bat";
      find = "fd";
      tree = "broot";
      monitor = "btm";
    };
    #interactiveShellInit = "starship init fish | source";
    #shellInit = "starship init fish | source";
  };

  xsession = {
    enable = true;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = "/home/rucy/.config/xmonad/xmonad.hs";
      extraPackages = hp: [ hp.dbus hp.monad-logger hp.xmonad-contrib ];
    };
  };
  programs.rofi = {
    enable = true;
    #theme = "/home/rucy/.config/rofi/nord.rasi";
  };
  services.picom = {
    enable = true;
    activeOpacity = "1.0";
    shadowExclude = [ "100:name *= 'zathura'" ];
    blur = true;
    backend = "glx";
    fade = true;
    fadeDelta = 5;
    shadow = true;
    shadowOpacity = "0.75";
  };
  services.dunst = {
    enable = false;
    iconTheme = {
      name = "Papirus";
      package = pkgs.papirus-icon-theme;
      size = "32x32";
    };
    settings = {
      global = {
        alignment = "left";
        bounce_freq = "0";
        browser = "firefox -new-tab";
        corner_radius = "2";
        dmenu = "rofi -dmenu -p dunst:";
        follow = "none";
        font = "Fira Sans 11";
        format = ''
          <b>%s</b>
          %b'';
        frame_color = "#1a1c25";
        frame_width = "1";
        geometry = "440x15-26+26";
        history_length = "20";
        horizontal_padding = "16";
        icon_position = "right";
        idle_threshold = "120";
        ignore_newline = "no";
        indicate_hidden = "yes";
        line_height = "0";
        markup = "full";
        max_icon_size = "64";
        monitor = "0";
        padding = "20";
        separator_color = "auto";
        separator_height = "4";
        show_age_threshold = "60";
        show_indicators = "yes";
        shrink = "no";
        sort = "yes";
        startup_notification = "false";
        sticky_history = "yes";
        transparency = "5";
        word_wrap = "yes";
      };
      urgency_low = {
        background = "#1E2029";
        foreground = "#bbc2cf";
        timeout = "8";
      };
      urgency_normal = {
        background = "#2a2d39";
        foreground = "#bbc2cf";
        timeout = "14";
      };
      urgency_critical = {
        background = "#cc6666";
        foreground = "#1E2029";
        timeout = "0";
      };
    };
  };

  services.screen-locker = {
    enable = true;
    inactiveInterval = 30;
    lockCmd = "${pkgs.betterlockscreen}/bin/betterlockscreen -l dimblur";
  };

  programs.zathura = {
    enable = true;
    options = {
      default-bg = "#2E3440";
      default-fg = "#3B4252";
      statusbar-fg = "#D8DEE9";
      statusbar-bg = "#434C5E";
      inputbar-bg = "#2E3440";
      inputbar-fg = "#8FBCBB";
      notification-bg = "#2E3440";
      notification-fg = "#8FBCBB";
      notification-error-bg = "#2E3440";
      notification-error-fg = "#BF616A";
      notification-warning-bg = "#2E3440";
      notification-warning-fg = "#BF616A";
      highlight-color = "#EBCB8B";
      highlight-active-color = "#81A1C1";
      completion-bg = "#3B4252";
      completion-fg = "#81A1C1";
      completion-highlight-fg = "#8FBCBB";
      completion-highlight-bg = "#81A1C1";
      recolor-lightcolor = "#2E3440";
      recolor-darkcolor = "#ECEFF4";
      recolor = "false";
      recolor-keephue = "false";
    };
  };

  # UwU ^_^
  programs.kitty = {
    enable = true;
    settings = {
      background_opacity = "1";
      shell = "fish";
      open_url_with = "firefox";
      font_family = "Fantasque Sans Mono Regular Nerd Font Complete Mono";
      bold_font = "Fantasque Sans Mono Bold Nerd Font Complete Mono";
      font_size = "12";
      enable_audio_bell = "no";
      window_alert_on_bell = "yes";
      cursor = "#d8dee9";
      foreground = "#d8dee9";
      background = "#2e3440";
      selection_foreground = "#2e3440";
      selection_background = "#d8dee9";
      color0 = "#282936";
      color1 = "#3a3c4e";
      color2 = "#4d4f68";
      color3 = "#626483";
      color4 = "#62d6e8";
      color5 = "#e9e9f4";
      color6 = "#f1f2f8";
      color7 = "#f7f7fb";
      color8 = "#ea51b2";
      color9 = "#b45bcf";
      color10 = "#00f769";
      color11 = "#ebff87";
      color12 = "#a1efe4";
      color13 = "#62d6e8";
      color14 = "#b45bcf";
      color15 = "#00f769";
    };
  };

  # Cool prompt
  programs.starship = {
    enable = true;
    enableFishIntegration = true;
    settings = {
      add_newline = true;
      character = {
        success_symbol = "[𝝺](#a3be8c)";
        error_symbol = "[𝝺](#bf616a)";
      };
    };
  };

  # Fuzzy search
  programs.fzf = {
    enable = true;
    enableFishIntegration = true;
  };

  # Alternative to MC and others

  programs.zoxide = {
    enable = true;
    enableFishIntegration = true;
  };

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      name = "emacs-overlay";
      url =
        "https://github.com/nix-community/emacs-overlay/archive/94ff7f128d913ae104e7b475e3777cef0d12f5a3.tar.gz";
      sha256 = "1fqhz214cjd274y2qiw3dxc9zplds56xzdgs68hbx64cg5xn6q0x";

    }))
  ];

  programs.direnv = {
    enable = true;
    enableFishIntegration = true;
  };
  programs.go = {
    enable = true;
    packages = {
      "github.com/motemen/gore/cmd/gore" =
        builtins.fetchGit "https://github.com/motemen/gore";
      "github.com/mdempsky/gocode" =
        builtins.fetchGit "https://go.googlesource.com/tools";
      "golang.org/x/tools/cmd/godoc" =
        builtins.fetchGit "https://go.googlesource.com/tools";
      "golang.org/x/tools/cmd/goimports" =
        builtins.fetchGit "https://go.googlesource.com/tools";
      "golang.org/x/tools/cmd/gorename" =
        builtins.fetchGit "https://go.googlesource.com/tools";
      "golang.org/x/tools/cmd/guru" =
        builtins.fetchGit "https://go.googlesource.com/tools";
      "github.com/cweill/gotests/..." =
        builtins.fetchGit "https://github.com/cweill/gotests";
      "github.com/fatih/gomodifytags" =
        builtins.fetchGit "https://github.com/fatih/gomodifytags";
      "github.com/mtchavez/skiplist" =
        builtins.fetchGit "https://github.com/mtchavez/skiplist";
      "github.com/ryszard/goskiplist" =
        builtins.fetchGit "https://github.com/ryszard/goskiplist";
      "github.com/sean-public/fast-skiplist" =
        builtins.fetchGit "https://github.com/sean-public/fast-skiplist";
      "github.com/MauriceGit/skiplist" =
        builtins.fetchGit "https://github.com/MauriceGit/skiplist";
      "github.com/google/btree" =
        builtins.fetchGit "https://github.com/google/btree";
      "google.golang.org/protobuf/cmd/protoc-gen-go" =
        builtins.fetchGit "https://github.com/protocolbuffers/protobuf-go";
      "google.golang.org/grpc/cmd/protoc-gen-go-grpc" =
        builtins.fetchGit "https://github.com/grpc/grpc-go";
      "github.com/gorilla/mux" =
        builtins.fetchGit "https://github.com/gorilla/mux";
      "github.com/spf13/cobra" =
        builtins.fetchGit "https://github.com/spf13/cobra";
      "github.com/stretchr/testify" =
        builtins.fetchGit "https://github.com/stretchr/testify";
      "github.com/lib/pq" = builtins.fetchGit "https://github.com/lib/pq";
      "github.com/c-bata/go-prompt" =
        builtins.fetchGit "https://github.com/c-bata/go-prompt";
      "github.com/sirupsen/logrus" =
        builtins.fetchGit "https://github.com/sirupsen/logrus";
    };
  };

  programs.bat.enable = true;

  services.lorri.enable = true;

  nixpkgs.config.allowUnfree = true;

  home.packages = with pkgs; [

    # My bar
    unstable.haskellPackages.xmobar

    # Citeproc citations DSL
    haskellPackages.pandoc-citeproc

    # Volume up and down
    playerctl

    # Brightness up and down
    brightnessctl

    # Wallpaper
    xwallpaper

    # Docker
    docker-compose

    # I dont know
    xidlehook
    killall
    libnotify
    acpi

    # Screenshots
    unstable.flameshot

    # To figure out key names for XMonad
    xorg.xev

    # Most important ricel tool
    neofetch

    # system monitoring
    unstable.bottom

    # Garbage electron apps that we are unfortunately forced to use
    zoom-us
    slack
    spotify
    teams
    #discord
    #gore

    # OBS
    obs-studio

    # Gtk stuff
    lxappearance
    nordic
    papirus-icon-theme
    dolphin

    # Lockscreen
    feh
    betterlockscreen

    # Emacs // huge
    binutils # native-comp needs 'as', provided by this
    emacsPgtkGcc # 28 + pgtk + native-comp

    ## Doom dependencies
    git
    (ripgrep.override { withPCRE2 = true; })
    gnutls # for TLS connectivity
    gcc

    ## Optional dependencies
    fd # faster projectile indexing
    imagemagick # for image-dired
    zstd # for undo-fu-session/undo-tree compression

    ## Nix doom
    nixfmt
    ## Roam doom
    sqlite
    pandoc
    texlive.combined.scheme-full

    ## Rust
    #unstable.rustfmt
    #unstable.rust-analyzer
    #unstable.rustc
    #unstable.cargo

    ## Python
    #python3
    #black
    #python38Packages.python-language-server
    #python38Packages.jupyter
    #graphviz

    ## C
    #gcc
    # ccls

    ## Extras
    unzip

    ## Haskell
    ghc-with-xmonad
    unstable.haskell-language-server

    ## Java
    unstable.jetbrains.idea-ultimate
    unstable.jetbrains.datagrip
    unstable.jetbrains.goland
    jdk8
    zip
    p7zip

    # RStudio
    RStudio-with-my-packages

    # rnix-lsp
    openssl

    # Smartid bs
    qdigidoc
    # Golang
    gopls
    golangci-lint
    # Rust utils
    exa
    tokei
    procs
    vlc
    openshot-qt
    # Runescape
    runelite

    arandr
    protobuf
    go-protobuf

    # kazam
    obsidian
    brave

    chrome-token-signing

    steam

    libpng

  ];

  home.stateVersion = "20.09";
}
