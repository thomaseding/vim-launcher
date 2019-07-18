# vim-launcher

.bashrc
```
v () {
  ARGS=$(vimlauncher "$@")
  if [ "$?" == '0' ]
  then
    vim $ARGS
  else
    echo "$ARGS" # https://stackoverflow.com/questions/16535886/maintain-line-breaks-in-output-from-subshell
    return "$?"
  fi
}
```
