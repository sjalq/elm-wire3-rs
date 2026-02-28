#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"

echo "=== Generating Rust types from Elm definitions ==="
cargo run --manifest-path ../../Cargo.toml -- elm/src/Messages.elm -o src/messages.rs

echo "=== Building Elm frontend ==="
cd elm
lamdera make src/Main.elm --output=../static/elm.js
cd ..

echo "=== Copying wire3-ws.js ==="
mkdir -p static/elm-pkg-js
cp ../../js/wire3-ws.js static/elm-pkg-js/

echo "=== Building Rust server ==="
cargo build

echo ""
echo "Done! Run with: cargo run"
echo "Then open http://localhost:3000"
