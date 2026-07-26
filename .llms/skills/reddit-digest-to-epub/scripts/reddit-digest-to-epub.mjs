#!/usr/bin/env node

import crypto from "node:crypto";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import process from "node:process";
import { chromium } from "playwright-core";
import JSZip from "jszip";

const SCHEMA_VERSION = 1;
const REDDIT_THREAD_RE =
  /\/(?:r\/AskHistorians\/)?comments\/([a-z0-9]+)(?:\/([^/?#]+))?(?:\/([a-z0-9]+))?/i;
const CP1252_BYTES = new Map([
  ["€", 0x80], ["‚", 0x82], ["ƒ", 0x83], ["„", 0x84], ["…", 0x85],
  ["†", 0x86], ["‡", 0x87], ["ˆ", 0x88], ["‰", 0x89], ["Š", 0x8a],
  ["‹", 0x8b], ["Œ", 0x8c], ["Ž", 0x8e], ["‘", 0x91], ["’", 0x92],
  ["“", 0x93], ["”", 0x94], ["•", 0x95], ["–", 0x96], ["—", 0x97],
  ["˜", 0x98], ["™", 0x99], ["š", 0x9a], ["›", 0x9b], ["œ", 0x9c],
  ["ž", 0x9e], ["Ÿ", 0x9f],
]);

class RedditThrottleError extends Error {
  constructor(message) {
    super(message);
    this.name = "RedditThrottleError";
  }
}

function usage() {
  return `Usage:
  node scripts/reddit-digest-to-epub.mjs DIGEST_URL [options]
  node scripts/reddit-digest-to-epub.mjs --self-test

Options:
  -o, --output PATH              Output EPUB path
      --headless                 Hide the browser (visible by default)
      --profile-dir PATH         Persistent browser profile directory
      --browser PATH             Chrome/Chromium executable
      --cache-dir PATH           Thread capture cache directory
      --refresh                  Ignore cached thread captures
      --concurrency N            Parallel browser pages (default: 1)
      --max-threads N            Limit digest links (default: all)
      --max-answer-threads N     Top-level answer branches per thread (default: 10)
      --max-comments N           Comments per thread (default: 120)
      --max-words-per-thread N   Answer words per thread (default: 45000)
      --max-expand-clicks N      "More comments" clicks per page (default: 160)
      --delay-ms N               Delay between pages (default: 2500)
      --timeout-ms N             Navigation timeout (default: 60000)
      --challenge-wait-ms N      Visible-browser block wait (default: 120000)
      --rate-limit-wait-ms N     Initial throttle cooldown (default: 90000)
      --rate-limit-retries N     Throttle retries per thread (default: 4)
  -h, --help                     Show help
`;
}

function positiveInt(value, name, { allowZero = false } = {}) {
  const n = Number.parseInt(value, 10);
  if (!Number.isFinite(n) || (allowZero ? n < 0 : n <= 0)) {
    throw new Error(`${name} must be ${allowZero ? "a non-negative" : "a positive"} integer`);
  }
  return n;
}

function parseArgs(argv) {
  const homeCache = path.join(os.homedir(), ".cache", "reddit-digest-to-epub");
  const opts = {
    url: null,
    output: null,
    headless: false,
    profileDir: path.join(homeCache, "browser-profile"),
    browser: process.env.REDDIT_EPUB_BROWSER || null,
    cacheDir: path.join(homeCache, "threads"),
    refresh: false,
    concurrency: 1,
    maxThreads: Infinity,
    maxAnswerThreads: 10,
    maxComments: 120,
    maxWordsPerThread: 45000,
    maxExpandClicks: 160,
    delayMs: 2500,
    timeoutMs: 60000,
    challengeWaitMs: 120000,
    rateLimitWaitMs: 90000,
    rateLimitRetries: 4,
    selfTest: false,
  };

  for (let i = 0; i < argv.length; i += 1) {
    const arg = argv[i];
    const next = () => {
      if (i + 1 >= argv.length) throw new Error(`${arg} requires a value`);
      i += 1;
      return argv[i];
    };
    if (arg === "-h" || arg === "--help") opts.help = true;
    else if (arg === "--self-test") opts.selfTest = true;
    else if (arg === "-o" || arg === "--output") opts.output = path.resolve(next());
    else if (arg === "--headless") opts.headless = true;
    else if (arg === "--profile-dir") opts.profileDir = path.resolve(next());
    else if (arg === "--browser") opts.browser = path.resolve(next());
    else if (arg === "--cache-dir") opts.cacheDir = path.resolve(next());
    else if (arg === "--refresh") opts.refresh = true;
    else if (arg === "--concurrency") opts.concurrency = positiveInt(next(), arg);
    else if (arg === "--max-threads") opts.maxThreads = positiveInt(next(), arg);
    else if (arg === "--max-answer-threads") opts.maxAnswerThreads = positiveInt(next(), arg);
    else if (arg === "--max-comments") opts.maxComments = positiveInt(next(), arg);
    else if (arg === "--max-words-per-thread") {
      opts.maxWordsPerThread = positiveInt(next(), arg);
    } else if (arg === "--max-expand-clicks") {
      opts.maxExpandClicks = positiveInt(next(), arg, { allowZero: true });
    } else if (arg === "--delay-ms") opts.delayMs = positiveInt(next(), arg, { allowZero: true });
    else if (arg === "--timeout-ms") opts.timeoutMs = positiveInt(next(), arg);
    else if (arg === "--challenge-wait-ms") opts.challengeWaitMs = positiveInt(next(), arg);
    else if (arg === "--rate-limit-wait-ms") opts.rateLimitWaitMs = positiveInt(next(), arg);
    else if (arg === "--rate-limit-retries") {
      opts.rateLimitRetries = positiveInt(next(), arg, { allowZero: true });
    }
    else if (arg.startsWith("-")) throw new Error(`Unknown option: ${arg}`);
    else if (opts.url) throw new Error(`Unexpected argument: ${arg}`);
    else opts.url = arg;
  }
  return opts;
}

function repairMojibake(value = "") {
  return String(value).replace(/(?:Ã.|Â.|Ä.|â..)/gu, (sequence) => {
    const bytes = [];
    for (const character of sequence) {
      const code = character.codePointAt(0);
      const byte = CP1252_BYTES.get(character) ?? (code <= 0xff ? code : null);
      if (byte === null) return sequence;
      bytes.push(byte);
    }
    try {
      return new TextDecoder("utf-8", { fatal: true }).decode(Uint8Array.from(bytes));
    } catch {
      return sequence;
    }
  });
}

function cleanText(value = "") {
  return repairMojibake(value)
    .replace(/[\u0000-\u0008\u000b\u000c\u000e-\u001f]/g, "")
    .replace(/\r\n?/g, "\n")
    .trim();
}

function escapeXml(value = "") {
  return cleanText(value)
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;")
    .replaceAll("'", "&apos;");
}

function slugify(value) {
  return cleanText(value)
    .normalize("NFKD")
    .replace(/[\u0300-\u036f]/g, "")
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-+|-+$/g, "")
    .slice(0, 90) || "askhistorians-sunday-digest";
}

function wordCount(value) {
  const plain = cleanText(value.replace(/<[^>]*>/g, " "));
  return plain ? plain.split(/\s+/u).length : 0;
}

function parseRedditUrl(rawUrl) {
  const url = new URL(rawUrl);
  const match = url.pathname.match(REDDIT_THREAD_RE);
  if (!match) throw new Error(`Not a Reddit comment-thread URL: ${rawUrl}`);
  return {
    postId: match[1].toLowerCase(),
    slug: match[2] || "_",
    targetCommentId: match[3]?.toLowerCase() || null,
  };
}

function canonicalBrowserUrl(rawUrl) {
  const parsed = parseRedditUrl(rawUrl);
  const suffix = parsed.targetCommentId ? `/${parsed.targetCommentId}/` : "/";
  const url = new URL(
    `https://www.reddit.com/r/AskHistorians/comments/${parsed.postId}/${parsed.slug}${suffix}`,
  );
  url.searchParams.set("sort", "top");
  return url.toString();
}

function cacheKey(item) {
  const fingerprint = crypto
    .createHash("sha1")
    .update([...item.sourceUrls].sort().join("\n"))
    .digest("hex")
    .slice(0, 12);
  return `${item.postId}-${fingerprint}.json`;
}

function findBrowserExecutable(requested) {
  const candidates = [
    requested,
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
    "/Applications/Chromium.app/Contents/MacOS/Chromium",
    "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge",
    "/usr/bin/google-chrome",
    "/usr/bin/google-chrome-stable",
    "/usr/bin/chromium",
    "/usr/bin/chromium-browser",
    "C:\\Program Files\\Google\\Chrome\\Application\\chrome.exe",
    "C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe",
  ].filter(Boolean);
  const found = candidates.find((candidate) => fs.existsSync(candidate));
  if (found) return found;
  try {
    const bundled = chromium.executablePath();
    if (fs.existsSync(bundled)) return bundled;
  } catch {
    // The package may not have a bundled browser; the message below explains setup.
  }
  throw new Error(
    "No Chrome/Chromium executable found. Install Chrome or pass --browser /absolute/path.",
  );
}

async function launchContext(opts) {
  fs.mkdirSync(opts.profileDir, { recursive: true });
  const executablePath = findBrowserExecutable(opts.browser);
  console.log(`Browser: ${executablePath}`);
  console.log(`Profile: ${opts.profileDir}`);
  return chromium.launchPersistentContext(opts.profileDir, {
    executablePath,
    headless: opts.headless,
    viewport: { width: 1280, height: 900 },
    locale: "en-CA",
    userAgent:
      "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) " +
      "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/137.0.0.0 Safari/537.36",
  });
}

async function bodyLooksBlocked(page) {
  return page.evaluate(() => {
    const text = (document.body?.innerText || "").toLowerCase();
    return [
      "you've been blocked by network security",
      "you have been blocked by network security",
      "whoa there, pardner",
      "request has been blocked",
      "too many requests",
    ].some((phrase) => text.includes(phrase));
  });
}

async function navigate(page, rawUrl, opts) {
  const url = canonicalBrowserUrl(rawUrl);
  let response = null;
  let navigationError = null;
  for (let attempt = 1; attempt <= 3; attempt += 1) {
    try {
      response = await page.goto(url, {
        waitUntil: "domcontentloaded",
        timeout: opts.timeoutMs,
      });
      navigationError = null;
      break;
    } catch (error) {
      navigationError = error;
      // Chrome can throw for an HTTP error even though Reddit rendered a useful
      // block/challenge page. Preserve that page for the explicit check below.
      if (
        /ERR_HTTP_RESPONSE_CODE_FAILURE/i.test(error.message) ||
        await bodyLooksBlocked(page).catch(() => false)
      ) {
        break;
      }
      if (attempt < 3) await page.waitForTimeout(attempt * 2500);
    }
  }
  await page.waitForTimeout(1200);

  if (response && [403, 429].includes(response.status()) || (await bodyLooksBlocked(page))) {
    if (!opts.headless) {
      console.warn(
        `Reddit blocked the initial request. Waiting up to ${Math.round(
          opts.challengeWaitMs / 1000,
        )}s for the visible browser session…`,
      );
      const deadline = Date.now() + opts.challengeWaitMs;
      while (Date.now() < deadline && (await bodyLooksBlocked(page))) {
        await page.waitForTimeout(2000);
      }
    }
    if (await bodyLooksBlocked(page)) {
      throw new RedditThrottleError(
        "Reddit returned a network-security block page. Try a normal home network, " +
          "leave the visible browser open for its JavaScript check, or reuse --profile-dir.",
      );
    }
  }
  if (navigationError && !response) {
    if (/ERR_HTTP_RESPONSE_CODE_FAILURE/i.test(navigationError.message)) {
      throw new RedditThrottleError(
        `Reddit refused the browser request: ${navigationError.message}`,
      );
    }
    throw new Error(`Browser navigation failed after 3 attempts: ${navigationError.message}`);
  }

  const title = await page.title();
  if (/page not found|not found/i.test(title)) throw new Error(`Reddit page not found: ${url}`);
  return url;
}

async function dismissBanners(page) {
  const labels = ["Reject non-essential", "Accept all", "Continue"];
  for (const label of labels) {
    const locator = page.getByRole("button", { name: label, exact: false });
    if ((await locator.count()) === 1 && (await locator.isVisible())) {
      await locator.click().catch(() => {});
      break;
    }
  }
}

async function expandRenderedComments(page, maxClicks) {
  if (maxClicks === 0) return 0;
  let clicks = 0;
  let stableRounds = 0;
  let previousHeight = 0;

  while (clicks < maxClicks && stableRounds < 3) {
    await page.evaluate(() => window.scrollTo(0, document.documentElement.scrollHeight));
    await page.waitForTimeout(500);
    const height = await page.evaluate(() => document.documentElement.scrollHeight);
    stableRounds = height === previousHeight ? stableRounds + 1 : 0;
    previousHeight = height;

    const buttons = page.locator('button, [role="button"]');
    const count = Math.min(await buttons.count(), 500);
    let clickedThisRound = 0;
    for (let i = 0; i < count && clicks < maxClicks && clickedThisRound < 12; i += 1) {
      const button = buttons.nth(i);
      if (!(await button.isVisible().catch(() => false))) continue;
      const text = cleanText(await button.innerText().catch(() => ""));
      if (!/(more repl|view more comment|load more comment|more comment|show more repl)/i.test(text)) {
        continue;
      }
      await button.click({ timeout: 3000 }).catch(() => {});
      clicks += 1;
      clickedThisRound += 1;
      await page.waitForTimeout(250);
    }
    if (clickedThisRound > 0) stableRounds = 0;
  }
  await page.evaluate(() => window.scrollTo(0, 0));
  return clicks;
}

async function extractRenderedPage(page) {
  return page.evaluate(() => {
    function deepAll(selector) {
      const found = [];
      const visit = (root) => {
        found.push(...root.querySelectorAll(selector));
        for (const el of root.querySelectorAll("*")) {
          if (el.shadowRoot) visit(el.shadowRoot);
        }
      };
      visit(document);
      return [...new Set(found)];
    }

    function text(value) {
      return String(value || "")
        .replace(/[\u0000-\u0008\u000b\u000c\u000e-\u001f]/g, "")
        .replace(/\r\n?/g, "\n")
        .trim();
    }

    function esc(value) {
      return text(value)
        .replaceAll("&", "&amp;")
        .replaceAll("<", "&lt;")
        .replaceAll(">", "&gt;")
        .replaceAll('"', "&quot;");
    }

    function sanitizedHtml(root) {
      if (!root) return "";
      const allowed = new Set([
        "p", "br", "em", "strong", "b", "i", "ul", "ol", "li", "blockquote",
        "code", "pre", "h2", "h3", "h4", "a", "sup", "sub", "del", "s", "hr",
      ]);
      const skip = new Set(["script", "style", "svg", "button", "form", "input", "img"]);
      const render = (node) => {
        if (node.nodeType === 3) return esc(node.nodeValue);
        if (node.nodeType !== 1) return "";
        const rawTag = node.tagName.toLowerCase();
        if (skip.has(rawTag)) return "";
        const children = [...node.childNodes].map(render).join("");
        if (!allowed.has(rawTag)) return children;
        if (rawTag === "br") return "<br />";
        if (rawTag === "hr") return "<hr />";
        const tag = rawTag === "b" ? "strong" : rawTag === "i" ? "em" : rawTag === "s" ? "del" : rawTag;
        if (tag === "a") {
          const href = node.getAttribute("href");
          if (!href || /^javascript:/i.test(href)) return children;
          let absolute;
          try {
            absolute = new URL(href, location.href).href;
          } catch {
            return children;
          }
          return `<a href="${esc(absolute)}">${children}</a>`;
        }
        return `<${tag}>${children}</${tag}>`;
      };
      return [...root.childNodes].map(render).join("").trim();
    }

    function firstIn(root, selectors) {
      for (const selector of selectors) {
        const found = root?.querySelector?.(selector);
        if (found) return found;
      }
      return null;
    }

    const post =
      deepAll("shreddit-post")[0] ||
      document.querySelector(".thing.link") ||
      document.querySelector("article");
    const title =
      text(post?.getAttribute?.("post-title")) ||
      text(firstIn(post, ["h1", "h2", "a.title"])?.textContent) ||
      text(document.querySelector('meta[property="og:title"]')?.content).replace(/\s*: r\/.*$/i, "") ||
      text(document.title).replace(/\s*: r\/.*$/i, "");
    const postBody = firstIn(post, [
      '[slot="text-body"]',
      '[id$="-post-rtjson-content"]',
      '[data-post-click-location="text-body"]',
      ".usertext-body .md",
      ".md",
    ]);
    const author =
      text(post?.getAttribute?.("author")) ||
      text(firstIn(post, [".author", '[data-testid="post_author_link"]'])?.textContent);
    const timestamp =
      text(post?.getAttribute?.("created-timestamp")) ||
      text(firstIn(post, ["time"])?.getAttribute?.("datetime"));

    const commentEls = deepAll("shreddit-comment");
    if (!commentEls.length) commentEls.push(...document.querySelectorAll(".thing.comment"));
    const comments = commentEls.map((el, index) => {
      const oldReddit = el.matches?.(".thing.comment");
      const body = firstIn(el, [
        '[slot="comment"]',
        '[id$="-comment-rtjson-content"]',
        ".usertext-body .md",
        ".md",
      ]);
      let depth = Number.parseInt(el.getAttribute("depth") || "", 10);
      if (!Number.isFinite(depth)) {
        depth = oldReddit
          ? [...el.querySelectorAll(":scope > .child .thing.comment")].length
          : 0;
        let parent = el.parentElement;
        depth = 0;
        while (parent) {
          if (parent.matches?.("shreddit-comment, .thing.comment")) depth += 1;
          parent = parent.parentElement;
        }
      }
      const rawId =
        el.getAttribute("thingid") ||
        el.getAttribute("comment-id") ||
        el.getAttribute("data-fullname") ||
        el.id ||
        `comment-${index + 1}`;
      const rawBody = text(body?.textContent);
      return {
        id: rawId.replace(/^t1_/, "").toLowerCase(),
        parentId: text(el.getAttribute("parentid") || el.getAttribute("parent-id")).replace(/^t1_/, ""),
        author:
          text(el.getAttribute("author")) ||
          text(firstIn(el, [".author", '[data-testid="comment_author_link"]'])?.textContent) ||
          "[deleted]",
        timestamp:
          text(el.getAttribute("created-timestamp")) ||
          text(firstIn(el, ["time"])?.getAttribute?.("datetime")),
        score: text(el.getAttribute("score") || firstIn(el, [".score"])?.textContent),
        permalink: text(el.getAttribute("permalink")),
        depth,
        order: index,
        html: sanitizedHtml(body),
        text: rawBody,
      };
    }).filter((comment) => {
      const lowered = comment.text.toLowerCase();
      return comment.text && lowered !== "[deleted]" && lowered !== "[removed]";
    });

    function anchorsWithin(containers) {
      const anchors = [];
      const seen = new Set();
      const visit = (root) => {
        for (const anchor of root.querySelectorAll("a[href]")) {
          if (!seen.has(anchor)) {
            seen.add(anchor);
            anchors.push(anchor);
          }
        }
        for (const el of root.querySelectorAll("*")) {
          if (el.shadowRoot) visit(el.shadowRoot);
        }
      };
      for (const container of containers.filter(Boolean)) visit(container);
      return anchors;
    }

    // Restrict digest discovery to the post and its comments. Reddit can render
    // unrelated recommendation cards elsewhere on the same page.
    const links = anchorsWithin([post, ...commentEls]).map((anchor) => ({
      href: anchor.href,
      text: text(anchor.textContent),
    }));
    return {
      title,
      author,
      timestamp,
      questionHtml: sanitizedHtml(postBody),
      questionText: text(postBody?.textContent),
      comments,
      links,
      pageUrl: location.href,
    };
  });
}

function digestItems(pageData, digestPostId, maxThreads) {
  const byPost = new Map();
  for (const link of pageData.links) {
    let parsed;
    try {
      const url = new URL(link.href);
      if (!/(^|\.)reddit\.com$/i.test(url.hostname)) continue;
      parsed = parseRedditUrl(link.href);
    } catch {
      continue;
    }
    if (parsed.postId === digestPostId) continue;
    let item = byPost.get(parsed.postId);
    if (!item) {
      if (byPost.size >= maxThreads) continue;
      item = {
        postId: parsed.postId,
        titleHint: link.text,
        sourceUrls: [],
        targetCommentIds: [],
      };
      byPost.set(parsed.postId, item);
    }
    const sourceUrl = canonicalBrowserUrl(link.href);
    if (!item.sourceUrls.includes(sourceUrl)) item.sourceUrls.push(sourceUrl);
    if (parsed.targetCommentId && !item.targetCommentIds.includes(parsed.targetCommentId)) {
      item.targetCommentIds.push(parsed.targetCommentId);
    }
  }
  return [...byPost.values()];
}

function selectComments(comments, targetCommentIds, opts) {
  if (!comments.length) return { comments: [], truncated: false, warning: "No rendered replies found." };
  const targets = new Set(targetCommentIds.map((id) => id.toLowerCase()));
  let selected = [];
  let warning = "";

  if (targets.size) {
    for (let i = 0; i < comments.length; i += 1) {
      if (!targets.has(comments[i].id)) continue;
      const baseDepth = comments[i].depth;
      selected.push(comments[i]);
      for (let j = i + 1; j < comments.length && comments[j].depth > baseDepth; j += 1) {
        selected.push(comments[j]);
      }
    }
    if (!selected.length) {
      warning = "The linked answer permalink was not identifiable; included top rendered answers.";
    }
  }

  if (!selected.length) {
    const baseDepth = Math.min(...comments.map((comment) => comment.depth));
    let branch = 0;
    for (const comment of comments) {
      if (comment.depth === baseDepth) branch += 1;
      if (branch > opts.maxAnswerThreads) break;
      selected.push(comment);
    }
  }

  selected = [...new Map(selected.map((comment) => [comment.id, comment])).values()]
    .sort((a, b) => a.order - b.order);
  const limited = [];
  let words = 0;
  for (const comment of selected) {
    const nextWords = wordCount(comment.text);
    if (limited.length >= opts.maxComments || (limited.length && words + nextWords > opts.maxWordsPerThread)) {
      break;
    }
    limited.push(comment);
    words += nextWords;
  }
  return { comments: limited, truncated: limited.length < selected.length, warning };
}

function mergeThreadCaptures(captures, item, opts) {
  const first = captures[0];
  const comments = [];
  const seen = new Set();
  for (const capture of captures) {
    for (const comment of capture.comments) {
      if (seen.has(comment.id)) continue;
      seen.add(comment.id);
      comments.push({ ...comment, order: comments.length });
    }
  }
  const selected = selectComments(comments, item.targetCommentIds, opts);
  return {
    postId: item.postId,
    title: first.title || item.titleHint || `Reddit thread ${item.postId}`,
    author: first.author,
    timestamp: first.timestamp,
    questionHtml: first.questionHtml,
    questionText: first.questionText,
    sourceUrls: item.sourceUrls,
    targetCommentIds: item.targetCommentIds,
    comments: selected.comments,
    truncated: selected.truncated,
    warning: selected.warning,
  };
}

async function scrapeItem(page, item, opts) {
  const captures = [];
  const urls = item.targetCommentIds.length ? item.sourceUrls : [item.sourceUrls[0]];
  for (const [index, url] of urls.entries()) {
    if (index) await delay(opts.delayMs);
    await navigate(page, url, opts);
    await dismissBanners(page);
    const clicked = await expandRenderedComments(page, opts.maxExpandClicks);
    if (clicked) console.log(`  expanded ${clicked} comment controls`);
    const capture = await extractRenderedPage(page);
    if (!capture.title) throw new Error(`Could not find a rendered Reddit post at ${url}`);
    captures.push(capture);
  }
  return mergeThreadCaptures(captures, item, opts);
}

async function scrapeItemWithBackoff(page, item, opts, index, total) {
  for (let retry = 0; ; retry += 1) {
    try {
      return await scrapeItem(page, item, opts);
    } catch (error) {
      if (!(error instanceof RedditThrottleError) || retry >= opts.rateLimitRetries) throw error;
      const waitMs = Math.min(opts.rateLimitWaitMs * (2 ** retry), 10 * 60 * 1000);
      console.warn(
        `  [${index + 1}/${total}] Reddit throttled this session. ` +
          `Cooling down for ${formatDuration(waitMs)} before retry ` +
          `${retry + 1}/${opts.rateLimitRetries}…`,
      );
      await delay(waitMs);
    }
  }
}

function delay(ms) {
  const jitter = Math.floor(Math.random() * Math.min(350, Math.max(1, ms / 3)));
  return new Promise((resolve) => setTimeout(resolve, ms + jitter));
}

function formatDuration(ms) {
  const seconds = Math.max(1, Math.round(ms / 1000));
  if (seconds < 60) return `${seconds}s`;
  const minutes = Math.floor(seconds / 60);
  const remainder = seconds % 60;
  return remainder ? `${minutes}m ${remainder}s` : `${minutes}m`;
}

function cachedItemCount(items, opts) {
  if (opts.refresh) return 0;
  let count = 0;
  for (const item of items) {
    const cachePath = path.join(opts.cacheDir, cacheKey(item));
    if (!fs.existsSync(cachePath)) continue;
    try {
      const cached = JSON.parse(fs.readFileSync(cachePath, "utf8"));
      if (cached.schemaVersion === SCHEMA_VERSION) count += 1;
    } catch {
      // Ignore unreadable cache entries; the worker will replace them.
    }
  }
  return count;
}

function estimateRuntime(itemCount, cachedCount, opts) {
  const uncachedCount = Math.max(0, itemCount - cachedCount);
  if (uncachedCount === 0) {
    return { lowMinutes: 1, highMinutes: 3, uncachedCount };
  }
  const lanes = Math.max(1, Math.min(opts.concurrency, uncachedCount));
  const delaySeconds = opts.delayMs / 1000;
  const lowMinutes = Math.max(
    2,
    Math.ceil((uncachedCount * (delaySeconds + 4)) / lanes / 60),
  );
  const baseHigh = Math.ceil((uncachedCount * (delaySeconds + 15)) / lanes / 60);
  const likelyCooldowns = Math.floor(uncachedCount / 75);
  const cooldownAllowance = Math.ceil(
    (likelyCooldowns * opts.rateLimitWaitMs * 1.5) / 60000,
  );
  return {
    lowMinutes,
    highMinutes: Math.max(lowMinutes + 2, baseHigh + cooldownAllowance),
    uncachedCount,
  };
}

function chapterXhtml(thread, index) {
  const title = escapeXml(thread.title);
  const source = escapeXml(thread.sourceUrls[0]);
  const byline = [thread.author ? `Asked by u/${thread.author}` : "", thread.timestamp]
    .filter(Boolean)
    .join(" · ");
  const minDepth = thread.comments.length
    ? Math.min(...thread.comments.map((comment) => comment.depth))
    : 0;
  const replies = thread.comments.map((comment, replyIndex) => {
    const depth = Math.min(6, Math.max(0, comment.depth - minDepth));
    const meta = [
      comment.author ? `u/${comment.author}` : "Anonymous",
      comment.timestamp,
      comment.score && !/hidden/i.test(comment.score) ? comment.score : "",
    ].filter(Boolean).join(" · ");
    const heading = comment.depth === minDepth ? `Answer ${replyIndex + 1}` : "Reply";
    return `<section class="comment depth-${depth}" id="comment-${escapeXml(comment.id)}">
      <h3>${escapeXml(heading)}</h3>
      <p class="byline">${escapeXml(meta)}</p>
        <div class="prose">${repairMojibake(comment.html)}</div>
    </section>`;
  }).join("\n");
  const notices = [
    thread.warning ? `<p class="notice">${escapeXml(thread.warning)}</p>` : "",
    thread.truncated
      ? `<p class="notice">This thread was shortened at the configured comment or word limit.</p>`
      : "",
  ].join("");
  return xhtmlDocument(
    title,
    `<article class="chapter">
      <p class="chapter-number">Chapter ${index + 1}</p>
      <h1>${title}</h1>
      ${byline ? `<p class="byline">${escapeXml(byline)}</p>` : ""}
      <p class="source"><a href="${source}">Original Reddit thread</a></p>
      <section class="question">
        <h2>Question</h2>
        <div class="prose">${repairMojibake(thread.questionHtml) || "<p><em>No question description was provided.</em></p>"}</div>
      </section>
      ${notices}
      <div class="answer-separator">Answers</div>
      ${replies || '<p class="notice">No readable rendered replies were available.</p>'}
    </article>`,
  );
}

function xhtmlDocument(title, body, extraHead = "") {
  return `<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml" lang="en" xml:lang="en">
<head>
  <meta charset="utf-8" />
  <title>${escapeXml(title)}</title>
  <link rel="stylesheet" type="text/css" href="styles.css" />
  ${extraHead}
</head>
<body>
${body}
</body>
</html>`;
}

function navXhtml(book) {
  const entries = book.threads.map(
    (thread, index) =>
      `<li><a href="chapter-${String(index + 1).padStart(3, "0")}.xhtml">${escapeXml(thread.title)}</a></li>`,
  ).join("\n");
  const failures = book.failures.length
    ? `<li><a href="unavailable.xhtml">Unavailable threads (${book.failures.length})</a></li>`
    : "";
  return xhtmlDocument(
    book.title,
    `<header class="book-header">
      <p class="kicker">r/AskHistorians</p>
      <h1>${escapeXml(book.title)}</h1>
      <p>${book.threads.length} questions collected for offline reading</p>
    </header>
    <nav epub:type="toc" id="toc" xmlns:epub="http://www.idpf.org/2007/ops">
      <h2>Contents</h2>
      <ol>${entries}${failures}</ol>
    </nav>
    <section class="about">
      <h2>About this edition</h2>
      <p>Questions and rendered replies were collected from Reddit. Formatting was simplified for comfortable ebook reading. Usernames and original-thread links are retained for attribution.</p>
    </section>`,
  );
}

function unavailableXhtml(failures) {
  const items = failures.map(
    (failure) =>
      `<li><a href="${escapeXml(failure.url)}">${escapeXml(failure.title || failure.url)}</a>: ${escapeXml(failure.error)}</li>`,
  ).join("\n");
  return xhtmlDocument(
    "Unavailable threads",
    `<article><h1>Unavailable threads</h1><p>These links could not be captured.</p><ul>${items}</ul></article>`,
  );
}

function stylesCss() {
  return `body {
  color: #211d18;
  background: #fffdf8;
  font-family: Georgia, "Times New Roman", serif;
  line-height: 1.55;
  margin: 5%;
}
h1, h2, h3 { color: #382d24; line-height: 1.2; }
h1 { font-size: 1.75em; margin: 0.3em 0 0.6em; }
h2 { font-size: 1.3em; margin-top: 1.8em; }
h3 { font-size: 1em; margin: 0; }
a { color: #714126; }
ol li { margin: 0.55em 0; }
.book-header { border-bottom: 2px solid #9b775b; padding: 8vh 0 1.5em; }
.kicker, .chapter-number { color: #895b3e; font-family: sans-serif; font-size: 0.78em; font-weight: bold; letter-spacing: 0.13em; text-transform: uppercase; }
.byline, .source { color: #6c625a; font-family: sans-serif; font-size: 0.78em; }
.question { background: #f5eee4; border-left: 0.3em solid #9b775b; margin: 1.5em 0; padding: 0.3em 1.1em 1em; }
.answer-separator { border-bottom: 1px solid #b9aa9d; color: #895b3e; font-family: sans-serif; font-size: 0.8em; font-weight: bold; letter-spacing: 0.13em; margin: 2.5em 0 1.5em; padding-bottom: 0.4em; text-transform: uppercase; }
.comment { border-top: 1px solid #ded5cc; margin: 1.4em 0 0; padding-top: 1em; }
.comment.depth-1, .comment.depth-2, .comment.depth-3, .comment.depth-4, .comment.depth-5, .comment.depth-6 { border-left: 2px solid #d6c5b6; padding-left: 1em; }
.comment.depth-2, .comment.depth-3, .comment.depth-4, .comment.depth-5, .comment.depth-6 { margin-left: 1em; }
.prose p { margin: 0.7em 0; }
.prose blockquote { border-left: 3px solid #b9aa9d; color: #514942; margin-left: 0; padding-left: 1em; }
.prose pre, .prose code { font-family: monospace; white-space: pre-wrap; }
.notice { background: #fff3cf; border: 1px solid #dec77a; padding: 0.7em; }
.about { margin-top: 3em; }
`;
}

function contentOpf(book, modified) {
  const chapterManifest = book.threads.map(
    (_, index) =>
      `<item id="chapter-${index + 1}" href="chapter-${String(index + 1).padStart(3, "0")}.xhtml" media-type="application/xhtml+xml"/>`,
  ).join("\n");
  const chapterSpine = book.threads.map(
    (_, index) => `<itemref idref="chapter-${index + 1}"/>`,
  ).join("\n");
  const unavailableManifest = book.failures.length
    ? '<item id="unavailable" href="unavailable.xhtml" media-type="application/xhtml+xml"/>'
    : "";
  const unavailableSpine = book.failures.length ? '<itemref idref="unavailable"/>' : "";
  return `<?xml version="1.0" encoding="UTF-8"?>
<package xmlns="http://www.idpf.org/2007/opf" version="3.0" unique-identifier="book-id">
  <metadata xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:dcterms="http://purl.org/dc/terms/">
    <dc:identifier id="book-id">${escapeXml(book.identifier)}</dc:identifier>
    <dc:title>${escapeXml(book.title)}</dc:title>
    <dc:creator>r/AskHistorians contributors</dc:creator>
    <dc:language>en</dc:language>
    <dc:source>${escapeXml(book.sourceUrl)}</dc:source>
    <meta property="dcterms:modified">${modified}</meta>
  </metadata>
  <manifest>
    <item id="nav" href="nav.xhtml" media-type="application/xhtml+xml" properties="nav"/>
    <item id="css" href="styles.css" media-type="text/css"/>
    ${chapterManifest}
    ${unavailableManifest}
  </manifest>
  <spine>
    <itemref idref="nav"/>
    ${chapterSpine}
    ${unavailableSpine}
  </spine>
</package>`;
}

async function makeEpub(book) {
  const zip = new JSZip();
  zip.file("mimetype", "application/epub+zip", { compression: "STORE" });
  zip.file(
    "META-INF/container.xml",
    `<?xml version="1.0" encoding="UTF-8"?>
<container version="1.0" xmlns="urn:oasis:names:tc:opendocument:xmlns:container">
  <rootfiles><rootfile full-path="OEBPS/content.opf" media-type="application/oebps-package+xml"/></rootfiles>
</container>`,
  );
  zip.file("OEBPS/styles.css", stylesCss());
  zip.file("OEBPS/nav.xhtml", navXhtml(book));
  book.threads.forEach((thread, index) => {
    zip.file(
      `OEBPS/chapter-${String(index + 1).padStart(3, "0")}.xhtml`,
      chapterXhtml(thread, index),
    );
  });
  if (book.failures.length) zip.file("OEBPS/unavailable.xhtml", unavailableXhtml(book.failures));
  const modified = new Date().toISOString().replace(/\.\d{3}Z$/, "Z");
  zip.file("OEBPS/content.opf", contentOpf(book, modified));
  return zip.generateAsync({
    type: "nodebuffer",
    mimeType: "application/epub+zip",
    compression: "DEFLATE",
    compressionOptions: { level: 9 },
  });
}

async function validateEpub(buffer, expectedChapters) {
  const zip = await JSZip.loadAsync(buffer);
  const required = [
    "mimetype",
    "META-INF/container.xml",
    "OEBPS/content.opf",
    "OEBPS/nav.xhtml",
    "OEBPS/styles.css",
  ];
  for (const name of required) {
    if (!zip.file(name)) throw new Error(`EPUB validation failed: missing ${name}`);
  }
  const mime = await zip.file("mimetype").async("string");
  if (mime !== "application/epub+zip") throw new Error("EPUB validation failed: bad mimetype");
  const chapters = Object.keys(zip.files).filter((name) => /^OEBPS\/chapter-\d+\.xhtml$/.test(name));
  if (chapters.length !== expectedChapters) {
    throw new Error(`EPUB validation failed: expected ${expectedChapters} chapters, found ${chapters.length}`);
  }
  const opf = await zip.file("OEBPS/content.opf").async("string");
  if (!/<package\b/.test(opf) || !/<spine\b/.test(opf)) {
    throw new Error("EPUB validation failed: invalid package document");
  }
}

async function selfTest(opts) {
  const executablePath = findBrowserExecutable(opts.browser);
  const browser = await chromium.launch({ executablePath, headless: true });
  try {
    const page = await browser.newPage();
    await page.setContent(`<!doctype html><html><head><title>Fixture</title></head><body>
      <shreddit-post post-title="Sunday Digest | Fixture" author="AutoModerator">
        <div slot="text-body"><p>Weekly highlights.</p></div>
        <a href="https://www.reddit.com/r/AskHistorians/comments/abc123/a_question/def456/">Featured answer</a>
      </shreddit-post>
      <shreddit-comment comment-id="root1" author="Historian" depth="0">
        <div slot="comment"><p>A long <strong>answer</strong>.</p></div>
        <shreddit-comment comment-id="part2" author="Historian" depth="1">
          <div slot="comment"><p>Part two.</p></div>
        </shreddit-comment>
      </shreddit-comment>
    </body></html>`);
    const data = await extractRenderedPage(page);
    if (data.title !== "Sunday Digest | Fixture") throw new Error("post title extraction failed");
    if (data.comments.length !== 2 || !data.comments[0].html.includes("<strong>")) {
      throw new Error("nested comment extraction failed");
    }
    const items = digestItems(data, "digest1", Infinity);
    if (items.length !== 1 || items[0].targetCommentIds[0] !== "def456") {
      throw new Error("digest link extraction failed");
    }
    const thread = mergeThreadCaptures(
      [{ ...data, title: "A question", questionHtml: "<p>Question body.</p>" }],
      { ...items[0], targetCommentIds: ["root1"] },
      opts,
    );
    if (thread.comments.length !== 2) throw new Error("target branch selection failed");
    const book = {
      title: "Sunday Digest | Fixture",
      identifier: "urn:reddit-digest:test",
      sourceUrl: "https://www.reddit.com/r/AskHistorians/comments/digest1/fixture/",
      threads: [thread],
      failures: [],
    };
    const epub = await makeEpub(book);
    await validateEpub(epub, 1);
    const estimate = estimateRuntime(253, 0, {
      ...opts,
      concurrency: 1,
      delayMs: 2500,
      rateLimitWaitMs: 90000,
    });
    if (estimate.lowMinutes < 20 || estimate.highMinutes < 60) {
      throw new Error("runtime estimate is implausibly narrow for a large live digest");
    }
    console.log(`Self-test passed (${epub.length.toLocaleString()} byte EPUB).`);
  } finally {
    await browser.close();
  }
}

async function main() {
  let opts;
  try {
    opts = parseArgs(process.argv.slice(2));
  } catch (error) {
    console.error(error.message);
    console.error(usage());
    process.exitCode = 2;
    return;
  }
  if (opts.help) {
    console.log(usage());
    return;
  }
  if (opts.selfTest) {
    await selfTest(opts);
    return;
  }
  if (!opts.url) {
    console.error("A Sunday Digest URL is required.\n");
    console.error(usage());
    process.exitCode = 2;
    return;
  }

  const digestParsed = parseRedditUrl(opts.url);
  fs.mkdirSync(opts.cacheDir, { recursive: true });
  const context = await launchContext(opts);
  const page = context.pages()[0] || await context.newPage();
  const failures = [];
  const threads = [];
  try {
    console.log(
      "Preflight estimate: a large uncached digest commonly takes 30–90 minutes; " +
        "loading the digest index now for a calibrated range.",
    );
    console.log("Loading Sunday Digest…");
    const sourceUrl = await navigate(page, opts.url, opts);
    await dismissBanners(page);
    const expanded = await expandRenderedComments(page, opts.maxExpandClicks);
    console.log(`Expanded ${expanded} digest comment controls.`);
    const digestData = await extractRenderedPage(page);
    if (!digestData.title) throw new Error("Could not find the rendered Sunday Digest post.");
    const items = digestItems(digestData, digestParsed.postId, opts.maxThreads);
    if (!items.length) {
      throw new Error(
        "No linked r/AskHistorians threads were found. The digest may be blocked, " +
          "not fully rendered, or its comments may need manual expansion.",
      );
    }
    console.log(`Found ${items.length} unique linked threads.`);
    const cachedCount = cachedItemCount(items, opts);
    const estimate = estimateRuntime(items.length, cachedCount, opts);
    console.log(
      `Estimated runtime: ${estimate.lowMinutes}–${estimate.highMinutes} minutes ` +
        `(${estimate.uncachedCount} uncached, ${cachedCount} cached, ` +
        `${Math.min(opts.concurrency, Math.max(1, estimate.uncachedCount))} browser lane(s)).`,
    );
    console.log(
      "This is a planning range; Reddit challenges or repeated cooldowns can extend it. " +
        "Completed threads are cached for safe resume.",
    );

    const threadResults = new Array(items.length);
    let nextIndex = 0;
    async function worker(workerId) {
      const workerPage = workerId === 0 ? page : await context.newPage();
      if (workerId) await delay(workerId * 500);
      while (true) {
        const index = nextIndex;
        nextIndex += 1;
        if (index >= items.length) break;
        const item = items[index];
        const label = item.titleHint || item.postId;
        console.log(`[${index + 1}/${items.length}] ${label}`);
        const cachePath = path.join(opts.cacheDir, cacheKey(item));
        try {
          let thread;
          if (!opts.refresh && fs.existsSync(cachePath)) {
            const cached = JSON.parse(fs.readFileSync(cachePath, "utf8"));
            if (cached.schemaVersion === SCHEMA_VERSION) {
              thread = cached.thread;
              console.log(`  [${index + 1}] cache hit`);
            }
          }
          if (!thread) {
            await delay(opts.delayMs);
            thread = await scrapeItemWithBackoff(
              workerPage,
              item,
              opts,
              index,
              items.length,
            );
            fs.writeFileSync(
              cachePath,
              JSON.stringify({ schemaVersion: SCHEMA_VERSION, thread }, null, 2),
            );
          }
          threadResults[index] = thread;
        } catch (error) {
          console.warn(`  [${index + 1}] skipped: ${error.message}`);
          failures.push({ title: label, url: item.sourceUrls[0], error: error.message });
        }
      }
      if (workerId) await workerPage.close();
    }
    const workerCount = Math.min(opts.concurrency, items.length);
    await Promise.all(Array.from({ length: workerCount }, (_, index) => worker(index)));
    threads.push(...threadResults.filter(Boolean));

    if (!threads.length) throw new Error("None of the linked threads could be captured.");
    const bookTitle = digestData.title || "AskHistorians Sunday Digest";
    const output = opts.output || path.resolve(`${slugify(bookTitle)}.epub`);
    const book = {
      title: bookTitle,
      identifier: `urn:reddit-digest:${digestParsed.postId}`,
      sourceUrl,
      threads,
      failures,
    };
    const epub = await makeEpub(book);
    await validateEpub(epub, threads.length);
    fs.mkdirSync(path.dirname(output), { recursive: true });
    fs.writeFileSync(output, epub);
    console.log("");
    console.log(`Created: ${output}`);
    console.log(`Chapters: ${threads.length}`);
    console.log(`Unavailable: ${failures.length}`);
    console.log(`Size: ${(epub.length / 1024 / 1024).toFixed(2)} MiB`);
  } finally {
    await context.close();
  }
}

main().catch((error) => {
  console.error(`Error: ${error.message}`);
  process.exitCode = 1;
});
