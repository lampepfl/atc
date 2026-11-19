#!/usr/bin/env sh
# Start ATC: (re)builds the distribution when its sources changed, loads .env, runs it.
#
#   ./start.sh                     # interactive, in the current directory
#   ./start.sh -C ~/proj -m gpt    # any atc flag is passed through
#   ./start.sh -p "run: 1 + 1"     # one non-interactive turn
#   ./start.sh -Xmx4g              # -Xmx/-Xms go to the JVM, everything else to atc
#
# Needs bash for the `atc` wrapper, which starts the JVM.
# Environment (see .env.example): ANTHROPIC_API_KEY, OPENAI_API_KEY, ATC_MODEL,
# ATC_CONFIG, ATC_CWD, ATC_JAVA_OPTS, ATC_DEBUG, ATC_ENV_FILE, ATC_SKIP_BUILD.
set -eu

ROOT=$(CDPATH='' cd -- "$(dirname "$0")" && pwd)
ENV_FILE=${ATC_ENV_FILE:-$ROOT/.env}

# Load literal KEY=value entries without replacing non-empty exported values; with an
# argument, only that key.
load_env() {
  [ -f "$ENV_FILE" ] || return 0
  cr=$(printf '\r')
  while IFS= read -r line || [ -n "$line" ]; do
    line=${line%"$cr"}
    case "$line" in ''|'#'*) continue ;; esac
    line=${line#export }
    case "$line" in *=*) ;; *) continue ;; esac
    key=${line%%=*}
    value=${line#*=}
    case "$key" in ''|[0-9]*|*[!A-Za-z0-9_]*) continue ;; esac
    [ -z "${1:-}" ] || [ "$key" = "$1" ] || continue
    # strip surrounding quotes
    case "$value" in
      \"*\") value=${value#\"}; value=${value%\"} ;;
      \'*\') value=${value#\'}; value=${value%\'} ;;
    esac
    [ -z "$value" ] && continue
    eval "current=\${$key:-}"
    if [ -z "$current" ]; then
      export "$key=$value"
    fi
  done < "$ENV_FILE"
}

DIST="$ROOT/out/dist.dest"
JAR="$DIST/atc.jar"
LIBJAR="$DIST/atc-lib.jar"
# Touched after each build. Mill leaves the jars alone when the content they are built from
# did not change, so a source only touched or checked out again would stay newer than them
# and make every start rebuild.
STAMP="$DIST/build.stamp"

# The build runs before the rest of .env is loaded, so a Mill server it starts does not keep
# the API keys in its environment.
load_env ATC_SKIP_BUILD
needs_build=0
if [ "${ATC_SKIP_BUILD:-0}" != "1" ]; then
  if [ ! -f "$JAR" ] || [ ! -f "$LIBJAR" ] || [ ! -f "$STAMP" ]; then
    needs_build=1
  # What the distribution is built from; the tests are not part of it.
  elif [ -n "$(find "$ROOT/build.mill" "$ROOT/app/src" "$ROOT/app/resources" "$ROOT/lib/src" \
      -type f -newer "$STAMP" | head -1)" ]; then
    needs_build=1
  fi
fi
if [ "$needs_build" = 1 ]; then
  echo "[start.sh] building distribution (./mill dist)..." >&2
  (cd "$ROOT" && ./mill dist >/dev/null)
  touch "$STAMP"
fi
load_env
if [ ! -f "$JAR" ] || [ ! -f "$LIBJAR" ]; then
  echo "[start.sh] no distribution in $DIST: run ./mill dist, or start without ATC_SKIP_BUILD=1" >&2
  exit 1
fi

[ -n "${ATC_MODEL:-}" ]  && set -- -m "$ATC_MODEL" "$@"
[ -n "${ATC_CONFIG:-}" ] && set -- -c "$ATC_CONFIG" "$@"
[ -n "${ATC_CWD:-}" ]    && set -- -C "$ATC_CWD" "$@"

# The `atc` wrapper runs the build as it runs a release, with its JVM options, the -Xmx/-Xms
# of the command line and the startup cache, here under out/dist.dest/startup/. It looks for
# no update, and the build carries no release marker.
ATC_CACHE_DIR="$DIST" ATC_CHECK_UPDATES=0 exec bash "$ROOT/atc" run "$@"
