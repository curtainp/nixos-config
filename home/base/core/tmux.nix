{ pkgs, ... }:
{
  programs.tmux = {
    enable = true;
    mouse = true;
    shell = "${pkgs.zsh}/bin/zsh";
    terminal = "kitty";
    keyMode = "vi";

    extraConfig = ''
      set-option -sa terminal-overrides ",xterm*:Tc"
      set-option -sa terminal-features ',xterm-kitty:RGB'

      set -g mouse on
      set -g base-index 1
      set -g pane-base-index 1
      set-window-option -g pane-base-index 1
      setw -g automatic-rename on # rename window to reflect current program
      set -g renumber-windows on  # renumber windows when a window is closed
      set-window-option -g mode-keys vi

      set -g set-titles on                        # set terminal title
      set -g set-titles-string "#h ❐ #S ● #I #W"

      # reduce the time when type prefix
      set -sg escape-time 0
      set -sg repeat-time 0

      # yazi
      set -g allow-passthrough on
      set -ga update-environment TERM
      set -ga update-environment TERM_PROGRAM

      bind | split-window -h -c "#{pane_current_path}"
      bind - split-window -v -c "#{pane_current_path}"

      # change the window size
      bind -r H resize-pane -L 10
      bind -r J resize-pane -D 10
      bind -r K resize-pane -U 10
      bind -r L resize-pane -R 10

      # Visual mode
      bind -T copy-mode-vi v send-keys -X begin-selection
      bind -T copy-mode-vi C-v send-keys -X rectangle-toggle
      bind -T copy-mode-vi y send-keys -X copy-selection-and-cancel
      bind -T copy-mode-vi Y send-keys -X copy-end-of-line

      bind o show-options -g
      bind q kill-window
      bind Tab last-window
      bind c new-window -ac "#{pane_current_path}"
      # Save entire tmux history to a file - file will be on machine where tmux is running
      bind * command-prompt -p "save history to filename:" -I "/tmp/tmux.history" "capture-pane -S -32769 ; save-buffer %1 ; delete-buffer"
      bind K send-keys "clear"\; send-keys "Enter"\; clear-history
      bind r source-file ~/.config/tmux/tmux.conf \; display-message "tmux config reload."
      bind s choose-tree -swZ
      bind w choose-tree -wZ

      # ----------------------
      # Status Bar
      # -----------------------
      set -g status-justify left
      set -g status-position top
            #bind C-p display-popup -E "ipython"
            bind C-r display-popup \
              -d "#{pane_current_path}" \
              -w 90% \
              -h 90% \
              -E "yazi"
            bind C-t display-popup \
              -d "#{pane_current_path}" \
              -w 75% \
              -h 75% \
              -E "zsh"
    '';

    plugins = with pkgs; [
      tmuxPlugins.vim-tmux-navigator
      tmuxPlugins.sensible
      tmuxPlugins.tokyo-night-tmux
    ];
  };
}
