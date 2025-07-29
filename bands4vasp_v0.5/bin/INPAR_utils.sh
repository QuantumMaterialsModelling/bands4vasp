#INPAR helper functions


get_inpar_value_by_line() {
  line_num="$1"

  # Check which INPAR file exists
  if [ -f "$tmpdir/$INPAR" ]; then
    inpar_file="$tmpdir/$INPAR"
  elif [ -f ".$tmpdir/$INPAR" ]; then
    inpar_file=".$tmpdir/$INPAR"
  else
    return 1  # Neither file exists
  fi

  # Extract the specified line from $inpar_file
  line=$(sed -n "${line_num}p" "$inpar_file")


  # Empty or commentary line → false
  case "$line" in
    "" | [[:space:]]* | [[:space:]]*\#* ) return 1 ;;
  esac

  # Remove leading whitespace for further evaluation
  line_trimmed=$(printf '%s' "$line" | sed 's/^[[:space:]]*//')

  # Remove all whitespace for comparison
  val_clean=$(printf '%s' "$line_trimmed" | tr -d '[:space:]')

  # Evaluate TRUE/FALSE values (case-insensitive)
  case "$val_clean" in
    .[Tt][Rr][Uu][Ee]. | .[Tt][Rr][Uu][Ee] | [Tt][Rr][Uu][Ee] )
      return 0 ;;  # shell-true
    .[Ff][Aa][Ll][Ss][Ee]. | .[Ff][Aa][Ll][Ss][Ee] | [Ff][Aa][Ll][Ss][Ee] )
      return 1 ;;  # shell-false
  esac

  # Return integer value (may be negative)
  case "$val_clean" in
    -*[!0-9]* | [!0-9]* ) ;;   # not purely numeric
    * )
      printf '%s\n' "$val_clean"
      return 0
      ;;
  esac

  # If not evaluable, return false
  return 1
}




get_inpar_value_by_name() {
  param="$1"

  line=$(grep -i "^$param[[:space:]]*=" "$outpar" | head -n1)

  [ -z "$line" ] && return 1

  # Extract value (to the right of '='), remove whitespace
  value=$(printf '%s' "$line" | sed 's/^[^=]*=[[:space:]]*//;s/[[:space:]]*$//')

  # empty or commentary → false
  case "$value" in
    \#* | "" ) return 1 ;;
  esac

  # evaluate TRUE/FALSE (case-insensitiv)
  val_clean=$(printf '%s' "$value" | tr -d '[:space:]')

  case "$val_clean" in
    .[Tt][Rr][Uu][Ee]. | .[Tt][Rr][Uu][Ee] | [Tt][Rr][Uu][Ee] )
      return 0 ;;  # "shell-true"
    .[Ff][Aa][Ll][Ss][Ee]. | .[Ff][Aa][Ll][Ss][Ee] | [Ff][Aa][Ll][Ss][Ee] )
      return 1 ;;  # "shell-false"
  esac

  # return integer
  case "$val_clean" in
    -*[!0-9]* | [!0-9]* ) ;;
    * )
      # Integer, print!
      printf '%s\n' "$val_clean"
      return 0
      ;;
  esac

  # not evaluable → false
  return 1
}



check_value_string() {
  value="$1"

  # empty or commentary → false
  case "$value" in
    "" | \#* ) return 1 ;;
  esac

  # erase whitespace
  val_clean=$(printf '%s' "$value" | tr -d '[:space:]')

  # test boolean-Strings  (TRUE/FALSE variants)
  case "$val_clean" in
    .[Tt][Rr][Uu][Ee]. | .[Tt][Rr][Uu][Ee] | [Tt][Rr][Uu][Ee] )
      return 0 ;;
    .[Ff][Aa][Ll][Ss][Ee]. | .[Ff][Aa][Ll][Ss][Ee] | [Ff][Aa][Ll][Ss][Ee] )
      return 1 ;;
  esac

  # not evaluable → false
  return 1
}
