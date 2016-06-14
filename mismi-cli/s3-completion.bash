_s3()
{
  local cur colonprefixes

  COMPREPLY=()
  cur=${COMP_WORDS[COMP_CWORD]}

  CMDLINE=(--bash-completion-index $COMP_CWORD)
  for arg in ${COMP_WORDS[@]}; do
    CMDLINE=(${CMDLINE[@]} --bash-completion-word $arg)
  done

  colonprefixes=${cur%"${cur##*:}"}

  COMPREPLY=( $(s3 "${CMDLINE[@]}") )

  local i=${#COMPREPLY[*]}
  while [ $((--i)) -ge 0 ]; do
    COMPREPLY[$i]=${COMPREPLY[$i]#"$colonprefixes"}
  done
}

complete -F _s3 s3
