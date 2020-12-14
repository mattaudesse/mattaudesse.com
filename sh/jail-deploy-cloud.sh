#!/usr/bin/env bash
#shellcheck disable=SC2001

set -e

BROOT="/usr/local/bastille"
JAIL_NAME="mattaudesse-com"
CLOUD=matt@mattaudesse.com
IMPORTS=/home/matt/jail/import
LATEST_IMG="$(find "$BROOT/backups" -name "$JAIL_NAME"_\*.xz |sort -r |head -n1)"
LATEST_SHA="$(echo "$LATEST_IMG" | sed 's/\.xz$/.sha256/')"


REPLACE=$(cat <<EOF
  is-active () {
    [ \"\$(jls -d name | awk '/^$JAIL_NAME$/')\" ]
  }

  is-active \
    && sudo bastille stop    "$JAIL_NAME"

  echo "Replacing $JAIL_NAME..." \
    && sudo bastille destroy "$JAIL_NAME" \
     ; sudo bastille import  "$IMPORTS"/"$(basename "$LATEST_IMG")" \
    && sudo bastille start   "$JAIL_NAME" \
    && sudo service -j       "$JAIL_NAME" "$JAIL_NAME" status
EOF
)


main () {
  [ ! -f "$LATEST_IMG" ] && echo "$LATEST_IMG doesn't exist!" && exit 1
  [ ! -f "$LATEST_SHA" ] && echo "$LATEST_SHA doesn't exist!" && exit 1

  ! sha256 -c "$(cat "$LATEST_SHA")" "$LATEST_IMG" &>/dev/null \
    && echo "$LATEST_IMG failed SHA256 checksum!" \
    && exit 1

  rsync -vzhr --progress --mkpath \
    "$LATEST_IMG" \
    "$LATEST_SHA" \
    "$CLOUD":"$IMPORTS"/

  ssh "$CLOUD" -t "echo \"$REPLACE\" | bash"
}

main
