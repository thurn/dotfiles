# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="/Users/dthurn/.oh-my-zsh"

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="dthurn"

# Set list of themes to load
# Setting this variable when ZSH_THEME=random
# cause zsh load theme from this variable instead of
# looking in ~/.oh-my-zsh/themes/
# An empty array have no effect
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)

source $ZSH/oh-my-zsh.sh

# Disable line wrap
tput rmam

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

source ~/.shell_config.sh

# -----------------------------------------------------------------------------
# Smart completion and history search
# -----------------------------------------------------------------------------

# Rich descriptions, forgiving matching, and grouped completion results.
# Oh My Zsh has already initialized Zsh's completion system at this point.
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*' matcher-list \
  'm:{a-zA-Z}={A-Za-z}' \
  'r:|[._-]=* r:|=*'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:descriptions' format '[%d]'
zstyle ':completion:*' menu no
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*:git-checkout:*' sort false

# Carapace supplies detailed subcommand/flag/argument completions and can fall
# back to native Zsh, Fish, and Bash completers when needed.
export CARAPACE_BRIDGES='zsh,fish,bash,inshellisense'
source <(carapace _carapace)

# Turn normal Tab completion into a fuzzy, grouped picker.
source /opt/homebrew/opt/fzf-tab/share/fzf-tab/fzf-tab.zsh
zstyle ':fzf-tab:*' switch-group '<' '>'
zstyle ':fzf-tab:*' continuous-trigger '/'
zstyle ':fzf-tab:*' fzf-flags \
  --height=55% \
  --layout=reverse \
  --border=rounded \
  --info=inline-right \
  --prompt='  ' \
  --pointer='▶' \
  --marker='✓' \
  --color='border:#5f87af,prompt:#87d7ff,pointer:#ffaf5f,marker:#87d787,hl:#87d7ff,hl+:#ffffff'
zstyle ':fzf-tab:complete:cd:*' fzf-preview \
  'eza --all --color=always --group-directories-first --icons=auto "$realpath" 2>/dev/null | head -200'
zstyle ':fzf-tab:complete:(bat|cat|less|vim|nvim|code):*' fzf-preview \
  'bat --color=always --style=numbers --line-range=:300 "$realpath" 2>/dev/null || eza --all --color=always "$realpath" 2>/dev/null'

# Smarter directory jumping: `z project-name`, or `zi` for an interactive list.
eval "$(zoxide init zsh)"

# Atuin records rich history metadata. It owns no keys by default here; Ctrl-S
# is the primary history search, while Ctrl-R remains a familiar fallback.
export ATUIN_NOBIND=true
eval "$(atuin init zsh)"
if [[ -o interactive ]]; then
  stty -ixon 2>/dev/null  # Allow terminals to deliver Ctrl-S to ZLE.
  bindkey -M emacs '^S' atuin-search
  bindkey -M emacs '^R' atuin-search
  bindkey -M viins '^S' atuin-search-viins
  bindkey -M viins '^R' atuin-search-viins
fi

# Ghost-text suggestions. Right arrow/End accepts the whole suggestion;
# Ctrl-Right accepts one word at a time in most terminals.
ZSH_AUTOSUGGEST_STRATEGY=(history completion)
ZSH_AUTOSUGGEST_USE_ASYNC=true
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=8'
source /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh

# Must stay last: it colors valid commands, errors, paths, quotes, and options.
source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
