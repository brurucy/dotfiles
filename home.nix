{ config, pkgs, ... }:
let unstable = import <unstable> { };
in {
  programs.home-manager.enable = true;

  home.username = "rucycarneiro";
  home.homeDirectory = "/Users/rucycarneiro";

  programs.git = {
    userEmail = "rucy.carneiro@pipedrive.com";
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
      set -x JAVA_HOME /usr/libexec/java_home
      set -x GOPRIVATE 'github.com/pipedrive/*'
      set -x KUBECONFIG $HOME/.kubeconfig

      fish_add_path -a $VOLTA_HOME/bin
      fish_add_path -a $DEVBOX_HOME/bin
      fish_add_path -a $CARGO_BIN/bin
      fish_add_path -a $JAVA_HOME

      bind \u0192 'forward-word'
      bind \u222B 'backward-word'

      set --universal fish_color_error ff6c6b
      set --universal fish_color_normal 118C53
      set --universal fish_color_keyword c678dd
      set --universal fish_color_command 51afef
      set --universal fish_color_quote DFDFDF
      set --universal fish_color_operator c678dd
      set --universal fish_color_escape 118C53
      set --universal fish_color_autosuggestion bbc2cf
      set --universal fish_color_option 46D9FF
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
        normal = { family = "Fantasque Sans Mono"; };
        bold = { family = "Fantasque Sans Mono"; };
        italic = { family = "Victor Mono"; };
        bold_italic = { family = "Victor Mono"; };
        size = 12;
        use_thin_strokes = false;
      };
      draw_bold_text_with_bright_colors = true;
      colors = {
        primary = {
          background = "#131826";
          foreground = "#0CCC68";
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
      set-option -g status-style bg='#282c34',fg='#bfbfbf'
      set-option -g status-interval 1

      set-option -g status-left '#[bg=#5699AF]#[fg=#282c34]#{?client_prefix,#[bg=#c678dd],} ○ '
      set-option -ga status-left '#[bg=#282c34]#[fg=#c678dd]#{?window_zoomed_flag, ⭤ ,    }'

      set-option -g window-status-current-format "#[fg=#282c34]#[bg=#bd93f9]#[fg=#f8f8f2]#[bg=#bd93f9] #I #W #[fg=#bd93f9]#[bg=#282c34]"
      set-option -g window-status-format "#[fg=#f8f8f2]#[bg=#282c34]#I #W #[fg=#282c34]"

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
        success_symbol = "[𝝺](#118C53)";
        error_symbol = "[𝝺](#ff665c)";
      };
    };
  };

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url =
        "https://github.com/nix-community/emacs-overlay/archive/d32cf21820f0c14bb4dc7911e4d8d81b891a8f82.tar.gz";
      sha256 = "045bqf15lxba75lzp95l1xg6ah5cl7p04gkyihsxl3a9j9ngr219";
    }))
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacsGit;
  };

  home.packages = [
    # Utilities
    pkgs.exa
    pkgs.bat
    unstable.ripgrep
    pkgs.tokei
    pkgs.zstd
    pkgs.bottom
    pkgs.jq
    unstable.nodePackages.snyk
    pkgs.nixfmt
    unstable.dhall-json
    pkgs.graphviz
    unstable.difftastic
    unstable.weechat

    # Extra dev dependencies
    unstable.nodePackages.prettier
    unstable.gcc11

    # Kubernetes related
    unstable.kubectl
    unstable.kubernetes-helm

    # Languages
    unstable.rustup
    unstable.go_1_18
    pkgs.cmake
    unstable.chez-racket
    unstable.dhall
    pkgs.python310

    # FPGA
    pkgs.haskellPackages.clash-ghc

    # Debuggers
    unstable.delve

    # Language servers
    pkgs.lua53Packages.digestif
    unstable.rnix-lsp
    pkgs.nodePackages.bash-language-server
    unstable.dhall-lsp-server
    unstable.gopls
    unstable.rust-analyzer
    unstable.nodePackages.typescript-language-server

    # Latex bloat
    unstable.texlive.combined.scheme-full

    # Fonts
    pkgs.victor-mono
    pkgs.fantasque-sans-mono
  ];

  home.stateVersion = "21.11";
}
