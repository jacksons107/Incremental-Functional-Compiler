# Project conventions

- Don't add code speculatively. If a function has no caller yet, don't
  write it on the theory that a future step will need it -- wait until
  that step actually needs it, then add it then. (e.g. a `Store.exists`
  helper was removed because nothing called it yet, even though a later
  planned step was expected to need it.)
