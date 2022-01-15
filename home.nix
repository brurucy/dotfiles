{ config, pkgs, ... }:
let
  unstable = import <unstable> {};
in {
  programs.home-manager.enable = true;

  home.username = "rucycarneiro";
  home.homeDirectory = "/Users/rucycarneiro";

  programs.git = {
    userEmail = "rucy.carneiro@pipedrive.com";
    userName = "brurucy";
    enable = true;
    extraConfig = {
      url = {
        "ssh://git@github.com/" = {
          insteadOf = "https://github.com/";
        };
      };
    };
  };

  programs.fish = {
    enable = true;
    interactiveShellInit =
      ''
      set -x VOLTA_HOME $HOME/.volta
      set -x GOPATH $HOME/go
      set -x DEVBOX_HOME $HOME/.local
      fish_add_path -a $VOLTA_HOME/bin
      fish_add_path -a $GOPATH/bin
      fish_add_path -a $DEVBOX_HOME/bin
      '';
  };

  programs.alacritty = {
    enable = true;
    settings = {
      shell = {
        program = "fish";
        args = [
          "-l"
          "-c"
          "tmux attach || tmux"
        ];
      };
      font = {
        normal = {
          family = "Victor Mono";
        };
        bold = {
          family = "Victor Mono";
        };
        italic = {
          family = "Victor Mono";
        };
        bold_italic = {
          family = "Victor Mono";
        };
        size = 13;
      };
      draw_bold_text_with_bright_colors = true;
      colors = {
        primary = {
          background = "#282C34";
          foreground = "#BBC2CF";
        };
        normal = {
          black = "#1B2229";
          red = "#FF6C6B";
          green = "#98BE65";
          yellow = "#ECBE7B";
          blue = "#2257A0";
          magenta = "#A9A1E1";
          cyan = "#5699AF";
          white = "#DFDFDF";
        };
        bright = {
          black = "#3F444A";
          red = "#DA8548";
          green = "#4DB5BD";
          yellow = "#ECBE7B";
          blue = "#51AFEF";
          magenta = "#C678DD";
          cyan = "#46D9FF";
          white = "0xffffff";
        };
      };
    };
  };

  programs.tmux = {
    enable = true;
    extraConfig =
      ''
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
        success_symbol = "[𝝺](#c678dd) [𝝺](#46D9FF) [𝝺](#44b9b1)";
        error_symbol = "[𝝺](#ff6655) [𝝺](#dd8844) [𝝺](#ECBE7B)";
      };
    };
  };

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/0d01d3b11249a471c80ab2c972646c4c809b8237.tar.gz;
      sha256 = "0w2xp1l2mby3hidc12qsls8hm9md1rar3yvrdlzfqy15gax87ir7";
    }))
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacsGcc;
  };

  home.packages = [
    # Utilities
    pkgs.exa
    pkgs.bat
    unstable.ripgrep

    # Extra dev dependencies
    pkgs.cmake

    # Kubernetes related
    unstable.kubectl
    unstable.kubernetes-helm
    
    # Languages
    pkgs.go_1_17
    unstable.rustup
    
    # Language servers
    pkgs.rnix-lsp
    unstable.texlab
   
    # Latex bloat
    unstable.texlive.combined.scheme-full

    # Fonts
    pkgs.victor-mono
    pkgs.fantasque-sans-mono
  ];
  
  home.stateVersion = "21.11";
}
