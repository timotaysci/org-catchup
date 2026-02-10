# org-catchup

An Emacs package for preparing 1:1 catch-up meetings. It keeps everything in plain org files and generates a dated agenda before each meeting by pulling in open actions and captured items.

## The idea

Between meetings you capture things as they come up — a risk to flag, a decision needed, a team win. When it's time to prep, `M-x org-catchup-new` assembles a ready-to-go agenda from your captures and open actions. During the call you fill in notes. After the call you move new actions to `actions.org` so they carry forward next time.

## Directory layout

```
org-catchup/
├── org-catchup.el          # the package
├── templates/
│   └── catchup.org         # template for new catch-up files
├── capture.org             # inbox for items captured between meetings
├── actions.org             # open action items (carry forward each week)
├── team.org                # team roster / reference
└── catchups/
    ├── 2026-02-10.org      # generated catch-up files, one per date
    ├── 2026-02-17.org
    └── ...
```

## Workflow

### Between meetings: capture items

Run `M-x org-catchup-capture`. It prompts for:

1. **Item text** — what you want to raise
2. **Tag** — which section it belongs in (pick one):

| Tag | Goes into section |
|---|---|
| `update` | Key Updates |
| `decision` | Decisions Needed |
| `risk` | Risks & Escalations |
| `highlight` | Team Highlights |
| `ask` | Resource & Support Asks |
| `action` | Notes & New Actions |

This appends a `** TODO` entry under `* Inbox` in `capture.org`. You can also edit `capture.org` directly (`M-x org-catchup-open-capture`).

### Before the meeting: generate the agenda

Run `M-x org-catchup-new`. It prompts for a date (defaults to today), then:

1. If `catchups/DATE.org` already exists, it just opens it
2. Otherwise it creates a new file from `templates/catchup.org` with:
   - **Open actions** pulled from `actions.org` — each TODO headline becomes a list item under "Open Actions from Previous". Items tagged `:from_YYYY_MM_DD:` show their origin date.
   - **Captured items** pulled from the `* Inbox` in `capture.org` — each TODO is routed to the matching section based on its tag.

Review the generated file and flesh out each section before the call.

### During the meeting

Fill in `* Notes & New Actions` as you go. Update other sections live if needed.

### After the meeting

1. Move any new action items into `actions.org` as TODO headlines
2. Mark completed actions as DONE in `actions.org` (so they stop carrying forward)
3. Run `M-x org-catchup-sweep-inbox` to mark all captured items as DONE (so they don't appear in the next catch-up)
4. Optionally fill in `* Retrospective`

## Commands

| Command | What it does |
|---|---|
| `M-x org-catchup-new` | Generate (or open) a dated catch-up file |
| `M-x org-catchup-capture` | Quick-add a tagged item to `capture.org` |
| `M-x org-catchup-sweep-inbox` | Mark all Inbox TODOs as DONE after a call |
| `M-x org-catchup-open-capture` | Open `capture.org` |
| `M-x org-catchup-open-actions` | Open `actions.org` |
| `M-x org-catchup-open-team` | Open `team.org` |

## Setup

The package is loaded via `use-package` in your init:

```elisp
(use-package org-catchup
  :load-path "/path/to/org-catchup/"
  :custom (org-catchup-directory "~/my-catchups/")
  :commands (org-catchup-new org-catchup-capture org-catchup-open-capture org-catchup-open-actions org-catchup-open-team))
```

The directory defaults to `~/org-catchup/`. All configuration uses `defcustom`, so you can also use `M-x customize-group RET org-catchup` to set options interactively.

## Example: actions.org

```org
#+TITLE: Actions

* TODO Chase budget approval from finance        :from_2026_02_03:
* TODO Share Q1 OKR draft with team
* DONE Set up new CI pipeline
```

Only TODO items are pulled in. DONE items are skipped. The `:from_2026_02_03:` tag renders as `(from 2026-02-03)` in the generated file.

## Example: capture.org

```org
#+TITLE: Capture Log

* Inbox
** TODO Platform migration is two days ahead of schedule       :update:
** TODO Need sign-off on vendor contract by Friday             :decision:
** TODO J. shipped the new dashboard solo                      :highlight:
```

When you run `org-catchup-new`, each item lands in its matching section in the generated catch-up file.
