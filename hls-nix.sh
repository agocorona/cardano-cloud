#!/bin/bash
# Forzamos la ejecución de HLS dentro del entorno de Nix de cardano-node
# Limitar memoria del proceso HLS via GHCRTS (más fiable que serverExtraArgs de VSCode)
exec nix develop --accept-flake-config /root/cardano-node -c env GHCRTS="-M1500m -A64m" haskell-language-server "$@"
