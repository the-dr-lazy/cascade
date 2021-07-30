#!/usr/bin/env bash
set -euo pipefail

SCRIPTS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
ROOT_DIR=$(dirname "$SCRIPTS_DIR")
SERVICES_DIR="$ROOT_DIR/services"

[[ "${CI:-false}" == "true" ]] || pre-commit install

"$SCRIPTS_DIR/cabal.sh"

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
