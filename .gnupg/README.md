# GnuPG - ~/.gnupg

## Backup/restore (export/import)

```sh
gpg -a --export >pubs.asc
gpg -a --export-secret-keys >privs.asc
gpg --export-ownertrust >otrust.txt
```

```sh
gpg --import privs.asc
gpg --import pubs.asc
gpg --import-ownertrust otrust.txt
```
