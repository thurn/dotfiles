#!/usr/bin/env bash

set -u

limit=10
excludes=()

usage() {
  cat <<'EOF'
Usage: longestSourceFiles.sh [-n COUNT] [-x SUBSTRING]...

Show the source files with the most lines beneath the current directory.

Options:
  -n, --limit COUNT       Number of files to show (default: 10)
  -x, --exclude SUBSTRING Exclude paths containing SUBSTRING (repeatable)
  -h, --help              Show this help
EOF
}

while (($#)); do
  case "$1" in
    -n|--limit)
      if (($# < 2)); then
        printf 'Error: %s requires a positive integer.\n' "$1" >&2
        exit 2
      fi
      limit=$2
      shift 2
      ;;
    --limit=*)
      limit=${1#*=}
      shift
      ;;
    -x|--exclude)
      if (($# < 2)) || [[ -z "$2" ]]; then
        printf 'Error: %s requires a non-empty substring.\n' "$1" >&2
        exit 2
      fi
      excludes+=("$2")
      shift 2
      ;;
    --exclude=*)
      value=${1#*=}
      if [[ -z "$value" ]]; then
        printf 'Error: --exclude requires a non-empty substring.\n' >&2
        exit 2
      fi
      excludes+=("$value")
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      printf 'Error: unknown option: %s\n' "$1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

case "$limit" in
  ''|*[!0-9]*|0)
    printf 'Error: limit must be a positive integer.\n' >&2
    exit 2
    ;;
esac

source_glob='*.{c,h,cc,cpp,cxx,hh,hpp,hxx,m,mm,cs,rs,go,zig,java,kt,kts,scala,groovy,ts,tsx,js,jsx,mjs,cjs,vue,svelte,astro,py,pyw,rb,php,swift,dart,ex,exs,erl,hrl,fs,fsx,clj,cljs,cljc,lua,pl,pm,r,sh,bash,zsh,fish,sql,sol,asm,s}'

list_source_files() {
  if command -v rg >/dev/null 2>&1; then
    # rg is dramatically faster in large repositories and respects ignore files.
    rg --files -0 --iglob "$source_glob"
  else
    # Portable fallback. Perl performs the extension check without spawning a
    # process for every file.
    find . \
      -type d \( \
        -name .git -o -name .hg -o -name .svn -o \
        -name node_modules -o -name vendor -o -name target -o \
        -name .venv -o -name venv -o -name __pycache__ -o \
        -name dist -o -name build -o -name coverage -o \
        -name .next -o -name .nuxt -o -name obj \
      \) -prune -o -type f -print0
  fi
}

# A single Perl process filters, counts, sorts, and retains only the requested
# number of results. Paths remain NUL-delimited until they are printed.
list_source_files | /usr/bin/perl -0 -e '
  use strict;
  use warnings;

  my ($limit, $using_rg, @excludes) = @ARGV;
  my $extension = qr/\.(?:c|h|cc|cpp|cxx|hh|hpp|hxx|m|mm|cs|rs|go|zig|java|kt|kts|scala|groovy|ts|tsx|js|jsx|mjs|cjs|vue|svelte|astro|py|pyw|rb|php|swift|dart|ex|exs|erl|hrl|fs|fsx|clj|cljs|cljc|lua|pl|pm|r|sh|bash|zsh|fish|sql|sol|asm|s)\z/i;
  my @best;

  while (defined(my $path = <STDIN>)) {
    chomp $path;
    $path =~ s{^\./}{};
    next if !$using_rg && $path !~ $extension;

    my $excluded = 0;
    for my $substring (@excludes) {
      if (index($path, $substring) >= 0) {
        $excluded = 1;
        last;
      }
    }
    next if $excluded;

    open(my $file, "<", $path) or next;
    my $lines = 0;
    {
      local $/ = "\n";
      $lines++ while <$file>;
    }
    close($file);

    push @best, [$lines, $path];
    @best = sort {
      $b->[0] <=> $a->[0] || $a->[1] cmp $b->[1]
    } @best;
    pop @best if @best > $limit;
  }

  printf "%8d  %s\n", $_->[0], $_->[1] for @best;
' "$limit" "$(command -v rg >/dev/null 2>&1 && printf 1 || printf 0)" "${excludes[@]}"
