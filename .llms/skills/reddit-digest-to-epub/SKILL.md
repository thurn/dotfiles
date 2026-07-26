---
name: reddit-digest-to-epub
description: Convert a Reddit r/AskHistorians Sunday Digest URL into a polished, self-contained EPUB containing every linked question and its rendered answer-comment branches. Use for AskHistorians digests, Reddit question-and-answer collections, offline airplane reading, or requests to archive a Reddit digest as an ebook without Reddit OAuth or API access.
---

# Reddit Digest to EPUB

Use the bundled Playwright scraper. It reads Reddit's rendered browser DOM; do not
replace it with Reddit OAuth, `.json` endpoints, or an API client.

## Run

1. Ensure dependencies exist:

   ```bash
   cd ~/.llms/skills/reddit-digest-to-epub
   npm install
   ```

2. Convert a digest:

   ```bash
   node scripts/reddit-digest-to-epub.mjs DIGEST_URL --output /absolute/path/book.epub
   ```

The browser is visible by default and uses a dedicated persistent profile in
`~/.cache/reddit-digest-to-epub/browser-profile`. Leave it open if Reddit performs
a JavaScript check. The script never asks for Reddit OAuth. Use `--headless` only
when the current network permits automated browsing without a challenge.

Before running, give the user a planning estimate. Use 10–30 minutes for a small
uncached digest and 30–90 minutes for a large digest with roughly 200–300 links.
Explain that Reddit challenges and cooldowns may extend the run. After discovering
the digest links, relay the script's calibrated estimate based on the unique thread
count and valid cache hits.

Completed thread captures are cached in `~/.cache/reddit-digest-to-epub/threads`,
so rerunning the same command resumes. Prefer the default single browser lane and
2.5-second inter-page delay. When Reddit throttles the browser, let the script
cool down and retry; do not rotate identities or bypass Reddit's rate limits.

When finished, tell the user they can share this with an iPhone via Airdrop.

## Useful controls

- `--max-threads N`: make a smaller preview or validation book.
- `--concurrency N`: use multiple browser pages for a large digest; keep the
  default of 1 if Reddit begins throttling.
- `--max-answer-threads N`: cap top-level answer branches per whole-thread link.
- `--max-comments N`: cap included comments per Reddit thread.
- `--max-words-per-thread N`: cap answer words per Reddit thread.
- `--refresh`: ignore cached captures.
- `--profile-dir PATH`: use a different dedicated browser profile.
- `--browser PATH`: select a Chrome/Chromium executable.
- `--headless`: hide the browser.
- `--rate-limit-wait-ms N`: set the initial throttle cooldown; later retries use
  bounded exponential backoff.
- `--rate-limit-retries N`: set throttle retries per thread.

Direct comment permalinks receive special treatment: include the linked answer and
all of its loaded nested continuation, rather than applying the ordinary
top-answer-branch selection first. If a digest links the same question more than
once, merge the captured comment branches into one chapter.

Always report the created EPUB path and any skipped links shown in the final
summary. Run `--self-test` after modifying the script. Do not claim a live
validation when Reddit returned a block page; the script treats block pages as
errors and will not silently create an empty EPUB.
