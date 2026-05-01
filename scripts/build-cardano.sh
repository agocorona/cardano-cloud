#!/usr/bin/env bash

set -euo pipefail  # Opcional, pero buena práctica para scripts complejos

# Cargar PATH de Nix
if [ -f /etc/profile.d/nix.sh ]; then
  source /etc/profile.d/nix.sh
fi

if [ ! -f /root/.cardano_built ]; then
  echo "▶ Primera compilación de Cardano Node..."

  printf "experimental-features = nix-command flakes\n\
      build-users-group =\n\
      substituters = https://cache.iog.io https://cache.nixos.org\n\
      trusted-public-keys = cache.iog.io-1:HPVKrDdmoQy8VHxFEkY0YtVvzGwMHgO4pY1GSwpMq7g= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJcM1QEOQJYv9j7hWc=\n\
      fallback = true\n" > /etc/nix/nix.conf

  nix develop --accept-flake-config /root/cardano-node -c bash -lc "
    cd /root/cardano-node &&
    cabal update &&
    cabal build all &&
    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
      BOOTSTRAP_HASKELL_MINIMAL=1 \
      BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
      BOOTSTRAP_HASKELL_INSTALL_HLS=1 \
      BOOTSTRAP_HASKELL_ADJUST_BASHRC=0 \
      sh
  "

  # Rutas de binarios (estas líneas eran las problemáticas con \r)
  NIX_BIN_DIR=$(dirname "$(find /nix/store -name nix -type f -print -quit)")
  CARDANO_CLI_DIR=$(dirname "$(find /nix/store -name cardano-cli -type f -print -quit)")
  CARDANO_NODE_DIR=$(dirname "$(find / -name cardano-node -type f -print -quit)")

  {
    echo "export NIX_BIN_DIR=$NIX_BIN_DIR"
    echo "export CARDANO_CLI=$CARDANO_CLI_DIR"
    echo "export CARDANO_NODE=$CARDANO_NODE_DIR"
    echo "export PATH=\$PATH:$NIX_BIN_DIR:$CARDANO_CLI_DIR:$CARDANO_NODE_DIR"
  } >> /root/.bashrc

  touch /root/.cardano_built
else
  echo "✔ Cardano Node ya compilado"
fi

exec bash