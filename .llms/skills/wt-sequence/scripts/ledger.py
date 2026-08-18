#!/usr/bin/env python3
"""Maintain wt-sequence state in a linked worktree's private Git directory."""

from __future__ import annotations

import argparse
import json
import os
import subprocess
import tempfile
from pathlib import Path
from typing import Any


SCHEMA_VERSION = 1


def git(worktree: Path, *args: str) -> str:
    result = subprocess.run(
        ["git", "-C", str(worktree), *args],
        check=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    return result.stdout.strip()


def canonical_worktree(value: str) -> Path:
    path = Path(value).expanduser().resolve(strict=True)
    if git(path, "rev-parse", "--is-inside-work-tree") != "true":
        raise SystemExit(f"not a Git worktree: {path}")
    root = Path(git(path, "rev-parse", "--show-toplevel")).resolve(strict=True)
    if root != path:
        raise SystemExit(f"pass the exact worktree root: {root}")
    return path


def ledger_path(worktree: Path) -> Path:
    return Path(
        git(
            worktree,
            "rev-parse",
            "--path-format=absolute",
            "--git-path",
            "wt-sequence/state.json",
        )
    )


def load(worktree: Path) -> tuple[Path, dict[str, Any]]:
    path = ledger_path(worktree)
    try:
        state = json.loads(path.read_text())
    except FileNotFoundError as error:
        raise SystemExit(f"ledger not found: {path}") from error
    if state.get("schema_version") != SCHEMA_VERSION:
        raise SystemExit(f"unsupported ledger schema: {state.get('schema_version')}")
    if Path(state.get("worktree", "")).resolve() != worktree:
        raise SystemExit("ledger worktree identity does not match the requested worktree")
    return path, state


def save(path: Path, state: dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    descriptor, temporary = tempfile.mkstemp(prefix="state.", suffix=".json", dir=path.parent)
    try:
        with os.fdopen(descriptor, "w") as handle:
            json.dump(state, handle, indent=2, sort_keys=True)
            handle.write("\n")
            handle.flush()
            os.fsync(handle.fileno())
        os.replace(temporary, path)
    finally:
        if os.path.exists(temporary):
            os.unlink(temporary)


def task(state: dict[str, Any], task_id: str) -> dict[str, Any]:
    for entry in state["tasks"]:
        if entry["id"] == task_id:
            return entry
    raise SystemExit(f"unknown task ID: {task_id}")


def optional_update(target: dict[str, Any], args: argparse.Namespace, fields: list[str]) -> None:
    for field in fields:
        value = getattr(args, field)
        if value is not None:
            target[field] = value


def command_init(args: argparse.Namespace) -> None:
    worktree = canonical_worktree(args.worktree)
    path = ledger_path(worktree)
    if path.exists():
        raise SystemExit(f"ledger already exists: {path}")
    tasks = json.loads(args.tasks_json)
    if not isinstance(tasks, list) or len(tasks) < 2:
        raise SystemExit("tasks-json must be an array containing at least two tasks")
    normalized = []
    seen = set()
    for entry in tasks:
        if not isinstance(entry, dict) or not isinstance(entry.get("id"), str):
            raise SystemExit("every task needs a string id")
        if not isinstance(entry.get("title"), str):
            raise SystemExit("every task needs a string title")
        if entry["id"] in seen:
            raise SystemExit(f"duplicate task ID: {entry['id']}")
        seen.add(entry["id"])
        normalized.append(
            {
                "id": entry["id"],
                "title": entry["title"],
                "status": "pending",
                "commit_oid": None,
                "candidate_id": None,
                "candidate_source_oid": None,
                "cleanup_policy": None,
                "review_url": None,
            }
        )
    branch = git(worktree, "branch", "--show-current")
    if not branch or branch != args.branch:
        raise SystemExit(f"worktree branch is {branch or 'detached'}, expected {args.branch}")
    state = {
        "schema_version": SCHEMA_VERSION,
        "sequence_id": args.sequence,
        "worktree": str(worktree),
        "branch": branch,
        "base_oid": args.base_oid,
        "status": "active",
        "review_task_id": None,
        "speculative_task_id": None,
        "stash_oid": None,
        "tasks": normalized,
        "runtime": {},
    }
    save(path, state)
    print(path)


def command_show(args: argparse.Namespace) -> None:
    _, state = load(canonical_worktree(args.worktree))
    print(json.dumps(state, indent=2, sort_keys=True))


def command_set_task(args: argparse.Namespace) -> None:
    worktree = canonical_worktree(args.worktree)
    path, state = load(worktree)
    entry = task(state, args.id)
    optional_update(
        entry,
        args,
        [
            "status",
            "commit_oid",
            "candidate_id",
            "candidate_source_oid",
            "cleanup_policy",
            "review_url",
        ],
    )
    if args.clear_candidate:
        entry["candidate_id"] = None
        entry["candidate_source_oid"] = None
    if args.clear_review_url:
        entry["review_url"] = None
    save(path, state)


def command_set_sequence(args: argparse.Namespace) -> None:
    worktree = canonical_worktree(args.worktree)
    path, state = load(worktree)
    optional_update(
        state,
        args,
        ["status", "base_oid", "review_task_id", "speculative_task_id", "stash_oid"],
    )
    for field in args.clear:
        state[field] = None
    save(path, state)


def command_runtime(args: argparse.Namespace) -> None:
    worktree = canonical_worktree(args.worktree)
    path, state = load(worktree)
    if args.remove:
        state["runtime"].pop(args.key, None)
    else:
        state["runtime"][args.key] = json.loads(args.value)
    save(path, state)


def command_set_plan(args: argparse.Namespace) -> None:
    worktree = canonical_worktree(args.worktree)
    path, state = load(worktree)
    requested = json.loads(args.tasks_json)
    if not isinstance(requested, list) or len(requested) < 2:
        raise SystemExit("tasks-json must be an array containing at least two tasks")
    existing = {entry["id"]: entry for entry in state["tasks"]}
    fixed = [entry["id"] for entry in state["tasks"] if entry["status"] != "pending"]
    requested_ids = [entry.get("id") for entry in requested if isinstance(entry, dict)]
    if requested_ids[: len(fixed)] != fixed:
        raise SystemExit("active and completed tasks must remain the ordered plan prefix")
    if len(requested_ids) != len(set(requested_ids)):
        raise SystemExit("task IDs must be unique")
    replacement = []
    for entry in requested:
        if not isinstance(entry.get("id"), str) or not isinstance(entry.get("title"), str):
            raise SystemExit("every task needs string id and title fields")
        current = existing.get(entry["id"])
        if current:
            current["title"] = entry["title"]
            replacement.append(current)
        else:
            replacement.append(
                {
                    "id": entry["id"],
                    "title": entry["title"],
                    "status": "pending",
                    "commit_oid": None,
                    "candidate_id": None,
                    "candidate_source_oid": None,
                    "cleanup_policy": None,
                    "review_url": None,
                }
            )
    state["tasks"] = replacement
    save(path, state)


def command_verify(args: argparse.Namespace) -> None:
    worktree = canonical_worktree(args.worktree)
    _, state = load(worktree)
    branch = git(worktree, "branch", "--show-current")
    if branch != state["branch"]:
        raise SystemExit(f"branch mismatch: found {branch or 'detached'}, expected {state['branch']}")
    git(worktree, "cat-file", "-e", f"{state['base_oid']}^{{commit}}")
    previous = state["base_oid"]
    for entry in state["tasks"]:
        oid = entry.get("commit_oid")
        if oid:
            git(worktree, "cat-file", "-e", f"{oid}^{{commit}}")
            parent = git(worktree, "rev-parse", f"{oid}^")
            if parent != previous:
                raise SystemExit(
                    f"task {entry['id']} parent is {parent}, expected sequence predecessor {previous}"
                )
            if entry.get("candidate_id") and entry.get("candidate_source_oid") != oid:
                raise SystemExit(f"task {entry['id']} candidate source differs from its commit")
            previous = oid
    git(worktree, "merge-base", "--is-ancestor", previous, "HEAD")
    print("ledger verified")


def parser() -> argparse.ArgumentParser:
    root = argparse.ArgumentParser()
    commands = root.add_subparsers(dest="command", required=True)

    initialize = commands.add_parser("init")
    initialize.add_argument("--worktree", required=True)
    initialize.add_argument("--sequence", required=True)
    initialize.add_argument("--branch", required=True)
    initialize.add_argument("--base-oid", required=True)
    initialize.add_argument("--tasks-json", required=True)
    initialize.set_defaults(handler=command_init)

    show = commands.add_parser("show")
    show.add_argument("--worktree", required=True)
    show.set_defaults(handler=command_show)

    set_task = commands.add_parser("set-task")
    set_task.add_argument("--worktree", required=True)
    set_task.add_argument("--id", required=True)
    set_task.add_argument("--status")
    set_task.add_argument("--commit-oid")
    set_task.add_argument("--candidate-id")
    set_task.add_argument("--candidate-source-oid")
    set_task.add_argument("--cleanup-policy", choices=["automatic", "retain-worktree"])
    set_task.add_argument("--review-url")
    set_task.add_argument("--clear-candidate", action="store_true")
    set_task.add_argument("--clear-review-url", action="store_true")
    set_task.set_defaults(handler=command_set_task)

    set_sequence = commands.add_parser("set-sequence")
    set_sequence.add_argument("--worktree", required=True)
    set_sequence.add_argument("--status")
    set_sequence.add_argument("--base-oid")
    set_sequence.add_argument("--review-task-id")
    set_sequence.add_argument("--speculative-task-id")
    set_sequence.add_argument("--stash-oid")
    set_sequence.add_argument(
        "--clear",
        action="append",
        choices=["review_task_id", "speculative_task_id", "stash_oid"],
        default=[],
    )
    set_sequence.set_defaults(handler=command_set_sequence)

    runtime = commands.add_parser("runtime")
    runtime.add_argument("--worktree", required=True)
    runtime.add_argument("--key", required=True)
    runtime_group = runtime.add_mutually_exclusive_group(required=True)
    runtime_group.add_argument("--value")
    runtime_group.add_argument("--remove", action="store_true")
    runtime.set_defaults(handler=command_runtime)

    set_plan = commands.add_parser("set-plan")
    set_plan.add_argument("--worktree", required=True)
    set_plan.add_argument("--tasks-json", required=True)
    set_plan.set_defaults(handler=command_set_plan)

    verify = commands.add_parser("verify")
    verify.add_argument("--worktree", required=True)
    verify.set_defaults(handler=command_verify)
    return root


def main() -> None:
    args = parser().parse_args()
    args.handler(args)


if __name__ == "__main__":
    main()
