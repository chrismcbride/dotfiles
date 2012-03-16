# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh
export TERM=xterm-256color

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="mcbride"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git extract history-substring-search dirpersist)

source $ZSH/oh-my-zsh.sh

autoload -U add-zsh-hook
add-zsh-hook chpwd do_ls_on_chdir
add-zsh-hook chpwd dirpersiststore

function do_ls_on_chdir() {
	ls; 
}

alias bc='bc -q -l';
alias rm='rm -I';
alias glog="git log --pretty=format:'%C(yellow)%h%Creset - %C(red)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --date=short --name-status"

gdelbranch() { git branch -D "$@" ; git push origin :heads/"$@" }
gchangeset() { git diff $(git merge-base develop "$@").."$@" }
compdef _git gdelbranch=git-branch
compdef _git gchangeset=git-checkout

# Customize to your needs...
export PATH=/usr/local/bin:/usr/local/sbin/:/bin:/sbin:/usr/bin:/usr/sbin:/home/chris/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/bin:/bin:/usr/local/games:/usr/games
