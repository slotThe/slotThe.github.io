---
title: Fix hanging GPG database lock
date: 2026-09-02
---

If you ever encounter a warning like

    gpg: Note: database_open «ID» waiting for lock (held by «ID») ...

when interacting with `gpg-agent`, or even doing something as simple as `gpg --list-keys`,
just delete the respective pubring lock and try again:[^1]

``` console
$ cd ~/.gnupg/public-keys.d
$ rm pubring.db.lock
```

---

Would be nice to find the underlying issue some day, but for now this seems sufficient.

[^1]: [Source](https://discuss.kde.org/t/kgpg-starts-with-invalid-argument-gpg-error/23029).
