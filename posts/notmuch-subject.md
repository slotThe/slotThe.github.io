---
title: "Notmuch: Warn on Empty Subjects"
date: 2023-07-30
tags: emacs
no-toc: true
---

Emacs's `notmuch` package has this fantastic concept of an *attachment check*:
adding `notmuch-mua-attachment-check` to `notmuch-mua-send-hook` will,
before sending the message,
check whether the regular expression in `notmuch-mua-attachment-regexp` matches.
If yes—and no attachment has been added—it will alert the user,
asking whether one really wants to send that email;
otherwise, everything goes through smoothly.
Due to some personal idiosyncrasies,
I needed a variant of this to check for empty subjects,
lest I become one of those people who sends emails like that.
As always, Emacs delivers.

<!--more-->

The code for `notmuch-mua-attachment-check` is relatively straightforward,
and worth a look if we want to imitate this kind of behaviour for other headers.
A simplified[^1] version goes as follows:

``` emacs-lisp
(defun notmuch-mua-attachment-check ()
  "Signal an error an attachement is expected but missing.

Signal an error if the message text indicates that an attachment
is expected but no MML referencing an attachment is found.

Typically this is added to `notmuch-mua-send-hook'."
  (when (and
         ;; When the message mentions attachment...
         (save-excursion
           (message-goto-body)
           ;; Limit search from reaching other possible parts of the message
           (let ((search-limit (search-forward "\n<#" nil t)))
             (message-goto-body)
             (re-search-forward notmuch-mua-attachment-regexp search-limit t)))
         ;; ...but doesn't have a part with a filename...
         (save-excursion
           (message-goto-body)
           (not (re-search-forward "^<#part [^>]*filename=" nil t)))
         ;; ...and that's not okay...
         (not
          (y-or-n-p "Attachment mentioned, but no attachment - is that okay?")))
    ;; ...signal an error.
    (error "Missing attachment")))
```

There is nothing fancy happening here,
so it's not terribly difficult to adapt it to other settings.
As I said in the beginning,
I need it to check for subjects,
due to the bad habit of only adding a subject once the email is already written—only sometimes I forget.
Instead of trying to change my habits—which is hard!—it sounds much easier to modify Emacs to suit my needs.

The strategy is exactly the same as for `notmuch-mua-attachment-check`;
check for a certain regular expression, whitespace, and say something if it matches the current subject:

``` emacs-lisp
(defun notmuch-mua-subject-check ()
  (or (save-excursion
        (message-goto-subject)
        (message-beginning-of-header t)
        (not (looking-at-p "[[:blank:]]*$")))
      (y-or-n-p "No subject given – still send?")
      (error "No subject")))
```

All one has to do is to execute this before sending a mail:

``` emacs-lisp
(add-hook 'notmuch-mua-send-hook #'notmuch-mua-attachment-check)
```

and we're good to go.

[^1]: Looking for an attachment is complicated insofar as there are some ways the regular expression might match,
      which however *don't* indicate that anything needs to be done.
      The real code looks at this by observing text properties of the matches.
      However, this is not important for what follows, so I simply omitted it.

      If you're interested:

      ``` emacs-lisp
      ;; When the message mentions attachment...
      (save-excursion
        (message-goto-body)
        ;; Limit search from reaching other possible
        ;; parts of the message
        (let ((search-limit
               (search-forward "\n<#" nil t)))
          (message-goto-body)
          (cl-loop
           while (re-search-forward
                   notmuch-mua-attachment-regexp
                   search-limit t)
           ;; For every instance of the "attachment"
           ;; string found, examine the text
           ;; properties.  If the text has either a
           ;; `face' or `syntax-table' property then
           ;; it is quoted text and should *not*
           ;; cause the user to be asked about a
           ;; missing attachment.
           if (let ((props (text-properties-at
                            (match-beginning 0))))
                (not (or (memq 'syntax-table props)
                         (memq 'face props))))
           return t
           finally return nil)))
      ```

      Alternatively, check the source code of `notmuch-mua-attachment-check` directly.
