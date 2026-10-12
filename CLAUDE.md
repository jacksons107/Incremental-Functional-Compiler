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

- Don't add fallback paths to preserve backward compatibility with how
  something used to work. This project isn't at the stage where old
  behavior needs to keep working -- when an invariant changes (e.g.
  "every compile now happens inside a project"), let old usage break
  loudly instead of quietly keeping a parallel no-project/legacy path
  alive. (e.g. `Driver.compile_to_c` was changed to call
  `Project.root_of` unconditionally and let `No_project_root` propagate,
  rather than catching it to fall back to the pre-persistence
  no-project compile path.)

- Keep comments short and descriptive, not prescriptive. State what
  the code does, in a sentence or two, especially where it's not
  obvious from reading it -- don't narrate the reasoning, the
  alternatives considered, or how it fits into a larger plan. If a
  comment is explaining why rather than what, it's probably too long.

- Never reference a plan document's step numbers (e.g. "6a", "step 5")
  in code, comments, or test names. `PLAN.md`/`TODO.md`-style files are
  working documents for this conversation, not committed/persisted
  project artifacts -- a step number means nothing to someone reading
  the code later, and the numbering itself can freely change. Describe
  what the code/test actually does instead.
