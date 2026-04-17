# Approved Command Inventory

The following commands are pre-approved for use in this workspace without requiring
per-invocation confirmation. They are standard, non-destructive, or reversible CLI
operations used in the normal contributor automation workflow.

## Approval Policy (Must Follow)

- Prefer approved top-level `make` targets first whenever an equivalent target exists.
- Do not run ad hoc commands that trigger manual approval prompts in this environment.
- If a needed workflow is not auto-approvable, add a clean helper script or `make`
	target in-repo, then use that wrapper.
- Keep helper wrappers deterministic, auditable, and documented in this file.

## Autonomy And Safety Defaults

- For vanilla workloads, execute approved workflows end-to-end without requiring user babysitting.
- Favor commands and targets that leave an auditable trail (`make`, `git`, `gh`) and report what ran.
- Prefer read-only preflight checks before write operations, then perform the minimal safe mutation.
- Avoid destructive or high-risk operations unless explicitly requested and confirmed.
- When an operation path is not auto-approvable, do not use it directly; add a helper target/script and use that instead.

---

## `gh` — GitHub CLI

### Read / query (always safe)
```
gh run list --branch <branch> --limit <n>
gh run view <run-id>
gh pr list --state open --json ...
gh pr view <number> --json ...
gh pr checks <number>
gh issue list --state open --limit <n> --json ...
gh issue view <number>
gh pr diff <number>
gh api graphql --field query=...
gh api /repos/{owner}/{repo}/pulls/{number}/reviews
```

### Comment / label / close (low-risk writes)
```
gh issue comment <number> --body "..."
gh issue close <number> --comment "..."
gh issue create --title "..." --body "..."
gh pr comment <number> --body "..."
gh pr review <number> --comment --body "..."
gh pr review <number> --approve
gh api graphql -f query='mutation { resolveReviewThread(...) { ... } }'
```

### Create / update (create a PR or branch)
```
gh pr create --title "..." --body "..." --base main --draft
gh pr ready <number>
gh pr merge <number> --squash --delete-branch
```

---

## `git` — Version control

### Read-only
```
git status -sb
git log --oneline [-n <n>] [--graph]
git diff [--stat] [HEAD] [<file>]
git branch [-a] [-v]
git show <ref>
git stash list
```

### Local mutations (reversible)
```
git add <file> ...
git add -p
git commit -m "..."
git commit --amend --no-edit
git checkout -b <branch>
git checkout <branch>
git stash [push | pop]
git restore <file>
git reset HEAD <file>
```

### Remote interactions (require confirmation for force-push / reset)
```
git fetch [--prune]
git pull --rebase
git push [-u origin <branch>]
```

> **Force-push and hard-reset always require explicit user confirmation before execution.**
> (`git push --force`, `git reset --hard`)

---

## `cmake` / `ctest` / `ninja` — Build system

```
cmake --preset <name>
cmake --build build [--target <target>]
cmake -B build -S . [options...]
ctest --test-dir build [-R <regex>] [-V]
ninja -C build [<target>]
```

---

## `make` — Top-level convenience targets

```
make [all]
make build
make test
make clean
```

---

## `clang-format` — Code formatter

```
clang-format -i <file> ...
clang-format --dry-run -Werror <file> ...
find src -name "*.cpp" -o -name "*.cppm" | xargs clang-format -i
```

---

## General shell utilities (read-only / safe transforms)

```
ls [-la] <path>
find <path> -name <pattern>
cat <file>
head / tail [-n <n>] <file>
grep [-r] <pattern> <path>
wc -l <file>
diff <file1> <file2>
echo "..."
pwd
which <command>
```

---

## Python / pip (workspace venv only)

```
source .venv/bin/activate
pip install -r requirements.txt
python <script>
```
