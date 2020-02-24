#!/bin/bash -pe

# See also: <lnk:~/.config/systemd/user/vdir-android-contacts.service>

# take newest contacts from backup and convert to separate vcard files. Add UID
# from N if available, otherwise EMAIL, write as import/$UID.vcf, unless the
# existing with same name already exists and contents are equal.
#
# If addressbook/$UID.vcf doesn't exist, create it from the import but strip out
# PHOTO base64 embed because khard doesn't like those (that is, python-vobject
# library doesn't handle that part of vcard 2.1 at all gracefully but crashes on
# read).

from_dir=/mnt/nextcloud/.Contacts-Backup # contain *.vcf backup files (each backup in its separate file) uploaded from android using nextcloud client
to_dir=$HOME/.local/share/dav/contacts/import # where but the raw imported contacts
to_dir_simple=$HOME/.local/share/dav/contacts/addressbook # also save simplified (no PHOTO) version here
stat_file=$to_dir/.last-sync.timestamp  # this file modification time is used to tell whether to sync or not
tmp=$to_dir/.vcf-sync.tmp

# Chooose which vcf to sync from
function choose_vcf(){
  args=( -type f )
  if [[ -e $stat_file ]]; then
    args+=( -newer "$stat_file" )
  fi
  vcf=$(find "$from_dir" "${args[@]}" | sort -r | head -n1)
  if [[ -z $vcf ]]; then
    echo no new vcf to parse >&2
    exit
  fi
  echo reading from backup file "$vcf" >&2
  echo "$vcf"
}

function process_bundle(){
  vcf=$1
  uid=

  while read -r ln; do
    # mark beginning of a new vcard netry
    if [[ $ln =~ ^BEGIN:VCARD ]]; then
      echo "$ln" > "$tmp"
      uid=""
      continue
    fi
    # continue writing current
    echo "$ln" >> "$tmp"

    # and if it was marking end of VCARD then finish that
    if [[ $ln =~ ^END:VCARD ]]; then
      process_vcard "$tmp" "$uid"
    elif [[ $ln =~ ^N[:\;] ]]; then
      uid=$(echo "$ln" | md5sum | cut -d\  -f1)
    elif [[ $ln =~ ^EMAIL[:\;] ]] && [[ -z $uid ]]; then
      uid=$(echo "$ln" | md5sum | cut -d\  -f1)
    fi
  done < "$vcf"
}

function process_vcard(){
  src=$1
  uid=$2
  dst=$to_dir/$uid.vcf
  dst_simple=$to_dir_simple/$uid.vcf

  # don't do anything if UID missing
  if [[ -z $uid ]]; then
    echo "Error: no N: or EMAIL in vCard, please fix" >&2
    cat "$src" >&2
    return 1
  fi

  # Insert UID into the vCard
  sed -i "3i UID:$uid" "$src"

  # if the dst is misssing or the files differ, then...
  if [[ ! -e $dst ]] || ! diff -q "$src" "$dst"; then
    # ...put full vCard in place locally
    mv -b "$src" "$dst"
    # ...put stripped version of it to other place for other uses
    gawk '/^PHOTO/ { next } /.*:.*/ { print $0 }' < "$dst" > "$dst_simple"
  fi
}

function main(){
  { if [[ $# -eq 0 ]]; then choose_vcf; else cat "$@"; fi } | while read -r vcf; do
    process_bundle "$vcf"
  done && touch "$stat_file"
}

if [[ $- != *i* ]] || [[ $# -gt 1 ]]; then
  main "$@"
fi
