```bash
# From your terminal:
$ curl -s \
    https://mattaudesse.com/matt-audesse-git.asc \
  | gpg --import

# If you have your own key (and assuming you don't
# suspect my web server has been compromised) you
# may optionally `lsign` me to avoid a warning:
$ gpg --edit-key 86E20EE655531286
gpg> lsign
gpg> save

# Finally:
$ git verify-commit <SHA>
```
