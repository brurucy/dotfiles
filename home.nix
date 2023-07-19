{ config, pkgs, ... }:
let unstable = import <unstable> { };
in {
  programs.home-manager.enable = true;

  home.username = "rucycarneiro";
  home.homeDirectory = "/Users/rucycarneiro";

  programs.git = {
    userEmail = "brurucy@gmail.com";
    userName = "brurucy";
    enable = true;
    extraConfig = {
      url = { "git@github.com:" = { insteadOf = "https://github.com/"; }; };
      url = {
        "https://github.com/rust-lang/crates.io-index" = {
          insteadOf = "https://github.com/rust-lang/crates.io-index";
        };
      };
    };
  };

  programs.fish = {
    enable = true;
    interactiveShellInit = ''
      set -x VOLTA_HOME $HOME/.volta
      set -x DEVBOX_HOME $HOME/.local
      set -x CARGO_BIN $HOME/.cargo
      set -x JAVA_HOME /opt/homebrew/Cellar/openjdk/17.0.2/libexec/openjdk.jdk/Contents/Home/bin
      set -x GOPRIVATE 'github.com/pipedrive/*'
      set -x KUBECONFIG $HOME/.kubeconfig

      fish_add_path -a $VOLTA_HOME/bin
      fish_add_path -a $DEVBOX_HOME/bin
      fish_add_path -a $CARGO_BIN/bin
      fish_add_path -a $JAVA_HOME

      bind \u0192 'forward-word'
      bind \u222B 'backward-word'

      set --universal fish_color_error ff6c6b
      set --universal fish_color_normal FFB000
      set --universal fish_color_keyword FFB000
      set --universal fish_color_command FFB000
      set --universal fish_color_quote FFB000
      set --universal fish_color_operator FFB000
      set --universal fish_color_escape FFB000
      set --universal fish_color_autosuggestion FFCC00
      set --universal fish_color_option FFB000
    '';
  };

  programs.alacritty = {
    enable = true;
    settings = {
      shell = {
        program = "fish";
        args = [ "-l" "-c" "tmux attach || tmux" ];
      };
      font = {
        normal = { family = "Berkeley Mono"; };
        bold = { family = "Berkeley Mono"; };
        italic = { family = "Berkeley Mono"; };
        bold_italic = { family = "Berkeley Mono"; };
        size = 16;
      };
      draw_bold_text_with_bright_colors = true;
      colors = {
        primary = {
          background = "#131826";
          foreground = "#FFB000";
        };
        normal = {
          black = "#131826";
          red = "#ff6c6b";
          green = "#0CCC68";
          yellow = "#ECBE7B";
          blue = "#51afef";
          magenta = "#c678dd";
          cyan = "#46D9FF";
          white = "#DFDFDF";
        };
        bright = {
          black = "#131826";
          red = "#ff665c";
          green = "#0CCC68";
          yellow = "#FCCE7B";
          blue = "#51afef";
          magenta = "#C57BDB";
          cyan = "#5cEfFF";
          white = "#bbc2cf";
        };
      };
    };
  };

  programs.tmux = {
    enable = true;
    extraConfig = ''
      set-option -g xterm-keys on
      set-window-option -g xterm-keys on
      set-option -g default-terminal "xterm-256color"
      set-option -g default-shell /Users/rucycarneiro/.nix-profile/bin/fish
      set-option -g default-command /Users/rucycarneiro/.nix-profile/bin/fish
      set-option -s escape-time 0
      set-option -g mouse on
      run-shell '\
      command -v reattach-to-user-namespace >/dev/null && \
      tmux set-option -g default-command \
      "$SHELL -c \"reattach-to-user-namespace -l \\\"$(basename "$SHELL")\\\"\"" \
      || true'
      unbind-key C-b
      set-option -g prefix `
      bind-key ` send-prefix
      set-option -g status-keys emacs
      set-window-option -g mode-keys emacs
      bind-key Up select-pane -U
      bind-key Down select-pane -D
      bind-key Left select-pane -L
      bind-key Right select-pane -R
      bind-key R source-file ~/.tmux.conf
      bind-key '\' swap-window
      bind-key < swap-window -t -1
      bind-key > swap-window -t +1
      bind-key I command-prompt -p 'Insert window at:' '      \
      run-shell "                                     \
      if tmux select-window -t %1; then           \
      tmux new-window -a;                     \
      tmux swap-window -s %1 -t \$((%1+1));   \
      else                                        \
      tmux new-window;                        \
      tmux move-window -t %1;                 \
      fi;                                         \
      tmux select-window -t #{window_id};         \
      tmux select-window -t %1;                   \
      "'
      set-option -g status-style bg='#FFB000',fg='#131826'
      set-option -g status-interval 1

      set-option -g status-left ""

      set-option -g window-status-current-format "#[bg=#FFB000]#[fg=#131826] #I #W "
      set-option -g window-status-format "#[bg=#FFCC00]#[fg=#131826] #I #W "

      set-option -g status-right ""
      set-option -g monitor-activity on
      set-option -g detach-on-destroy off
      set-option -g display-time 36000000
      bind-key c new-window -c "#{pane_current_path}"
      set-option -g renumber-windows on
      set-option -g base-index 1
      set-window-option -g pane-base-index 1

      bind-key % split-window -h -c "#{pane_current_path}"
      bind-key '"' split-window -v -c "#{pane_current_path}"

      set-option -g history-limit 10000
    '';
  };

  programs.starship = {
    enable = true;
    enableFishIntegration = true;
    settings = {
      character = {
        success_symbol = "[𝝺](#FFB000)";
        error_symbol = "[𝝺](#ff665c)";
      };
    };
  };

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url =
        "https://github.com/nix-community/emacs-overlay/archive/297107759dabf44f3d3bde8c98dfc736d16e77b1.tar.gz";
      sha256 = "05c4d23i2r38c8dwkxsjx5f4ni9r893473jyj81fvlnf9krrd1ps";
    }))
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacsGit;
  };

  home.packages = [
    # Utilities
    pkgs.lsd
    pkgs.bat
    unstable.ripgrep
    pkgs.tokei
    pkgs.zstd
    pkgs.bottom
    pkgs.jq
    unstable.nodePackages.snyk
    pkgs.nixfmt
    pkgs.graphviz
    unstable.difftastic
    unstable.weechat
    pkgs.nodePackages.pkg
    unstable.golangci-lint
    unstable.tree-sitter
    pkgs.coreutils
    pkgs.protobuf

    # Extra dev dependencies
    unstable.nodePackages.prettier

    # Kubernetes related
    unstable.kubectl
    unstable.kubernetes-helm
    # unstable.colima
    unstable.podman
    pkgs.qemu
    unstable.buildkit

    # Languages
    pkgs.rustup
    unstable.go
    pkgs.cmake
    pkgs.python310
    # Debuggers
    unstable.delve

    # Language servers
    pkgs.lua53Packages.digestif
    unstable.rnix-lsp
    unstable.gopls
    unstable.rust-analyzer
    unstable.nodePackages.typescript-language-server

    # Latex bloat
    unstable.texlive.combined.scheme-full

    # Font
    pkgs.victor-mono
  ];

  home.stateVersion = "21.11";
}
