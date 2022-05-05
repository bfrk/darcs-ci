#compdef darcs
## Darcs completion for zsh.
##
## Originally derived from a version by
## Copyright (C) 2009  Nicolas Pouillard

local -a darcs_options darcs_non_options darcs_arguments darcs_list_options

if (($CURRENT == 2)); then
  compadd -- $(darcs --commands)
else
  # advanced zsh (array) parameter expansion fu:
  # - ${(f)...} means split into array elements at line endings
  #   instead of white space
  # - ${arr:#pat} drops elements matching pat from arr, whereas
  #   ${(M)arr:#pat} drops non-matching elements
  # - ${arr/pat/repl} replaces pat with repl for all elements of arr
  # - ${arr[(i)val]} is reverse indexing: returns index of first
  #   occurrence of val in arr

  # save current word
  local current_word=$words[$CURRENT]
  # delete it from words
  words[$CURRENT]=()
  # find the index of the first option argument
  local first_opt=${words[(i)-*]}
  # insert --list-options right before the first option argument
  darcs_list_options=($words[1,$(($first_opt - 1))] --list-options $words[$first_opt,-1])
  # debugging help
  #print -l $darcs_list_options > ./debug_darcs_completion
  # execute command line with --list-options inserted and
  # split the result (stdout) at line endings
  darcs_arguments=(${(f)"$($darcs_list_options 2>/dev/null)"})
  case $current_word; in
    /*|./*|\~*|../*)
      _files
      ;;
    -*)
      darcs_options=(${${(M)darcs_arguments:#-*}/;/:})
      _describe '' darcs_options
      ;;
    *)
      case "${words[2]}"; in
        get|clone)
          _urls
          ;;
        *)
          darcs_non_options=(${darcs_arguments:#-*})
          _multi_parts -i -S ' ' / darcs_non_options
          ;;
      esac
      ;;
  esac
fi
