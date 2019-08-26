#-----------------------------------------------------------------------------
# File:          ~/.shell_prompt_mine.sh
#
# Source this file for a cool terminal prompt (PS1 & PS2)
#-----------------------------------------------------------------------------

__shell_prompt(){

   local git_completion="/usr/share/git/completion/git-completion.bash"
   local git_prompt="/usr/share/git/completion/git-prompt.sh"

   [[ -f "$git_completion" ]] && source "$git_completion" || echo "warning: $git_completion not available" >&2
   [[ -f "$git_prompt" ]] && source "$git_prompt" || echo "warning: $git_prompt not available" >&2

   GIT_PS1_SHOWDIRTYSTATE="enabled" # unstaged(*) and staged(+) in __git_ps1

   # terminal colors indexed
   # tip: say e.g. ${colnorm[3]@P} to interpret these outside $PS[123]
   local i= colnorm=() colbold=() colreset="\[\e[0m\]"
   for i in $(seq 0 7); do
      colnorm[$i]="\[\e[0;3${i}m\]"
      colbold[$i]="\[\e[1;3${i}m\]"
   done

   # list of distingiushed colors - choose pseudo-random: =${colopts[$((${SEED}%${#colopts}))]}
   local colopts=(${colnorm[@]:2} ${colbold[@]:1:6})

   local coln=${colbold[2]}
   local coltime=${colnorm[6]}
   local colpid=${colnorm[5]}
   local colpath=${colnorm[6]}
   local colgit=${colbold[5]}

   # hostname color
   local colhost_seed=$( (cat /etc/machine-id || hostname) 2>&- | cksum | cut -f1 -d' ')
   local colhost=${colopts[ $((colhost_seed % ${#colopts[@]})) ]}

   # username color
   if [ $EUID -eq "0" ]
   then local coluser=${colbold[1]}
   else local coluser=${colnorm[2]}
   fi

   # PROMPT_COMMAND: value executed as a command prior to each PS1 prompt
   PROMPT_COMMAND="RET=\$?; JOBS=\$(jobs|wc -l)"

   # PS1: shell ready to read a command
   PS1=$coln
   # ┌{time}
   PS1+="┌{$coltime\t$coln}"
   # ─[PID>JOBS]
   PS1+="─[$colpid\${$}$coln"'$([[ ! $JOBS -eq 0 ]] && printf ">\001\e[0;36m\002$JOBS")'"$coln]"
   # ─{user@hostname:cwd}
   PS1+="─{$coluser\u$coln@$colhost\H$coln:$colpath\w$coln}"
   #  (git_status)\n
   PS1+="$colgit\$(2>/dev/null __git_ps1 || true)$coln\n"
   # ├RET┄$  (or ┄#)
   PS1+=├'$([[ $RET -eq 0 ]] && C="0;32m"||C="0;31m";printf "\001\e[%s\002" $C)'"\$RET"
   PS1+="$coln┄\[\e["'$([[ $EUID -eq 0 ]] && printf "0;31m" || printf "1;32m")'"\]\\$"
   PS1+="${colreset} "

   # PS2: shell needs more input to complete a command
   PS2="$coln│$colreset"

   export PS1 PS2 PROMPT_COMMAND
}

__shell_prompt
