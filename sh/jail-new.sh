#!/usr/bin/env bash

set -e

BROOT="/usr/local/bastille"
JAIL_NAME="mattaudesse-com"
JAIL_PATH="$BROOT/jails/$JAIL_NAME"
TMPLS_DIR="$BROOT/templates/mattaudesse"
TMPL_SYMLINK="$TMPLS_DIR/mattaudesse.com"
PROJ_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")"/.. >/dev/null 2>&1 && pwd)"


is-active () {
  [ "$(jls -d name | awk "/^$JAIL_NAME$/")" ]
}


await-stop () {
  bastille stop "$JAIL_NAME"

  if is-active; then
    printf "Waiting for active jail to fully shut down"

    while is-active; do
      printf "."
      sleep 1
    done

    echo
  fi
}


main () {
  if [ ! -x "$BINARY" ]; then
    echo "$BINARY not found"
    exit 1
  fi

  mkdir -p "$TMPLS_DIR"
  [ -L $TMPL_SYMLINK ] || ln -s "$PROJ_DIR" $TMPL_SYMLINK

  is-active && await-stop

  bastille destroy "$JAIL_NAME" || true
  bastille create  "$JAIL_NAME" 12.2-RELEASE 10.10.10.1 lo1
  bastille start   "$JAIL_NAME"

  pkg -j "$JAIL_NAME" install -y libiconv libffi gmp ca_root_nss

  bastille sysrc "$JAIL_NAME" cron_enable="NO"
  bastille sysrc "$JAIL_NAME" virecover_enable="NO"

  bastille sysrc "$JAIL_NAME" mattaudesse_com_enable="YES"
  bastille sysrc "$JAIL_NAME" mattaudesse_com_db="/mnt/${JAIL_NAME}/db/mattaudesse.com.db"
  bastille sysrc "$JAIL_NAME" mattaudesse_com_conf="/mnt/${JAIL_NAME}/conf/mattaudesse.com.yaml"

  jexec "$JAIL_NAME" pw usermod root -h -
  printf "Disabled password-based logins for \`root\` account.\n\n"

  jexec "$JAIL_NAME" pw groupadd mattaudesse-com -g 2100
  printf "Created \`mattaudesse-com\` (GID: 2100) group.\n"

  jexec "$JAIL_NAME" pw useradd mattaudesse-com \
    -u 2100 \
    -c mattaudesse-com \
    -g mattaudesse-com \
    -d /nonexistent \
    -s /usr/sbin/nologin \
    -h -
  printf "Created \`mattaudesse-com\` (UID: 2100) account.\n\n"

  jexec "$JAIL_NAME" tzsetup America/New_York
  printf "Timezone updated to America/New_York.\n\n"

  await-stop

  mkdir -p "$JAIL_PATH"/root/usr/local/bin
  mkdir -p "$JAIL_PATH"/root/usr/local/etc/rc.d
  mkdir -p "$JAIL_PATH"/root/mnt/"$JAIL_NAME"/{conf,db}

  cp "$BINARY"                                 "$JAIL_PATH"/root/usr/local/bin
  cp "$PROJ_DIR"/etc/jail/rc.d/mattaudesse-com "$JAIL_PATH"/root/usr/local/etc/rc.d
  cp "$PROJ_DIR"/etc/jail/resolv.conf          "$JAIL_PATH"/root/etc
  cp "$PROJ_DIR"/etc/jail/jail.conf            "$JAIL_PATH"

  # Eliminate leftover entries from `bastille create` but squelch `grep` errors
  # triggered by questionable `update_fstab` subroutine. See:
  # https://github.com/BastilleBSD/bastille/blob/5e9e58d/usr/local/share/bastille/import.sh#L112
  truncate -s 0 "$JAIL_PATH"/fstab

  chmod    0444 "$JAIL_PATH"/root/etc/resolv.conf
  chmod -R 0555 "$JAIL_PATH"/root/mnt/"$JAIL_NAME"
  chmod -R 0555 "$JAIL_PATH"/root/usr/local/bin
  chmod -R 0555 "$JAIL_PATH"/root/usr/local/etc/rc.d

  bastille export "$JAIL_NAME"
  echo
}

main
