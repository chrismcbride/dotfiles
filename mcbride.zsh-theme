# Git-centric variation of the "fishy" theme.
# See screenshot at http://ompldr.org/vOHcwZg

ZSH_THEME_GIT_PROMPT_ADDED=""
ZSH_THEME_GIT_PROMPT_MODIFIED=""
ZSH_THEME_GIT_PROMPT_DELETED=""
ZSH_THEME_GIT_PROMPT_RENAMED=""
ZSH_THEME_GIT_PROMPT_UNMERGED=""
ZSH_THEME_GIT_PROMPT_UNTRACKED=""

ZSH_THEME_GIT_PROMPT_AHEAD="$fg_bold[yellow]↑"
ZSH_THEME_GIT_PROMPT_PREFIX=""
ZSH_THEME_GIT_PROMPT_SUFFIX=""
ZSH_THEME_GIT_PROMPT_DIRTY=" $fg_bold[red]✗"
ZSH_THEME_GIT_PROMPT_CLEAN=" %{$fg_bold[green]%}✔"

local user_color='green'
test $UID -eq 0 && user_color='red'

PROMPT='%(?..%{$fg_bold[red]%}exit %?
%{$reset_color%})'\
'%B%!%b %{$fg[$user_color]%}%~%{$reset_color%}'\
' $(git_prompt_status)%{$reset_color%}'\
'$fg_bold[magenta]$(git_prompt_info)%{$reset_color%}'\
'$(git_prompt_ahead)$reset_color'\
'%(!.#.>) '

PROMPT2='%{$fg[red]%}%_ %{$reset_color%}'
PROMPT3='%{$fg[red]%}... %{$reset_color%}'
RPROMPT='%{$fg_bold[yellow]%}(%D %*)%{$reset_color%}'
