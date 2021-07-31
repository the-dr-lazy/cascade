#!/usr/bin/env bash
set -euo pipefail

SCRIPTS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
# shellcheck source=scripts/variables.sh
source "$SCRIPTS_DIR/variables.sh"

"$GENERATOR_SCRIPTS_DIR/cabal.sh"
"$GENERATOR_SCRIPTS_DIR/hlint.sh"

if [[ "${CI:-false}" == "true" ]]; then
    printf "Short circuiting because of CI environment"
    exit 0
fi

case "$(uname)" in
    'Darwin')
        SERVICE_NAME="com.cascade.dev.agent.plist"
        SERVICE_PATH="$HOME/Library/LaunchAgents/$SERVICE_NAME"
        sed "s|<!--CASCADE_ROOT-->|$ROOT_DIR|g" "$SERVICES_DIR/osx/$SERVICE_NAME" > "$SERVICE_PATH"

        if launchctl list | grep -q com.cascade.dev.agent; then
            launchctl unload "$SERVICE_PATH"
        fi

        launchctl load "$SERVICE_PATH"
        printf "Cascade develpment agent has been loaded"
        ;;
esac
