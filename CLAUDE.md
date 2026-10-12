# Project conventions

- Don't add code speculatively. If a function has no caller yet, don't
  write it on the theory that a future step will need it -- wait until
  that step actually needs it, then add it then. (e.g. a `Store.exists`
  helper was removed because nothing called it yet, even though a later
  planned step was expected to need it.)

- Functions that operate on persisted state (a file, a store entry)
  should assume that state already exists and is valid, and fail loudly
  if it doesn't, rather than silently creating or repairing it. If
  creation is a real, separate operation, give it its own function
  instead of folding "create if missing" into a function that's
  otherwise meant to operate on existing state. (e.g. `Namespace.append`
  raises if the namespace file doesn't exist, rather than creating it;
  `Namespace.create` is the dedicated function for making a new one.)

- Don't leave a raw octal/numeric literal like a file permission mode
  inline at its call site -- give it a name so a reader doesn't have to
  decode it. (e.g. `rwxr_xr_x = 0o755` in `project.ml`, `rw_rw_rw = 0o666`
  in `namespace.ml`.)
