# Base Ubuntu
FROM ubuntu:22.04

# UTF-8 locale
ENV LANG en_US.UTF-8
ENV LC_ALL en_US.UTF-8

# Dependencias básicas
RUN apt-get update && apt-get install -y curl dos2unix xz-utils unzip git bash locales && \
    locale-gen en_US.UTF-8 && \
    mkdir -p /nix /etc/nix && chmod a+rwx /nix

# Usuario opcional
RUN adduser user --home /home/user --disabled-password --gecos "" --shell /bin/bash
USER user
ENV USER user
WORKDIR /home/user

# Install Nix in multi-user mode (recommended)
RUN curl -L https://nixos.org/nix/install | sh

# Switch back to root user
USER root
ENV USER root

# Detectar ruta real de nix y ponerla en PATH globalmente
RUN set -eux; \
    NIX_BIN_DIR=$(dirname $(find /nix/store -name nix -type f -print -quit)); \
    echo "export PATH=\$PATH:$NIX_BIN_DIR" > /etc/profile.d/nix.sh; \
    chmod +x /etc/profile.d/nix.sh; \
    export PATH="$PATH:$NIX_BIN_DIR"

# Clonar Cardano Node
# Recomendado: clonar y hacer checkout del tag exacto que usa cardano-api-10.19.1.0
RUN git clone https://github.com/IntersectMBO/cardano-node.git /root/cardano-node && \
    cd /root/cardano-node && \
    git fetch --all --tags && \
    git checkout tags/10.6.1 && \
    git submodule update --init --recursive
    
# Configuración global de Nix
RUN printf "experimental-features = nix-command flakes\n\
build-users-group =\n\
substituters = https://cache.iog.io https://cache.nixos.org\n\
trusted-public-keys = cache.iog.io-1:HPVKrDdmoQy8VHxFEkY0YtVvzGwMHgO4pY1GSwpMq7g= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJcM1QEOQJYv9j7hWc=\n\
fallback = true\n" > /etc/nix/nix.conf

# Instalar Ogmios
ARG OGMIO_VERSION=6.14.0
ARG OGMIO_ARCH=x86_64-linux
RUN curl -L "https://github.com/CardanoSolutions/ogmios/releases/download/v${OGMIO_VERSION}/ogmios-v${OGMIO_VERSION}-${OGMIO_ARCH}.zip" \
    -o /tmp/ogmios.zip && \
    mkdir -p /tmp/ogmios && unzip /tmp/ogmios.zip -d /tmp/ogmios && \
    find /tmp/ogmios -type f -name ogmios -exec mv {} /usr/local/bin/ogmios \; && \
    chmod +x /usr/local/bin/ogmios && rm -rf /tmp/ogmios /tmp/ogmios.zip

# Script build-cardano.sh (compilación diferida)
RUN cat << 'EOF' > /usr/local/bin/build-cardano.sh
#!/usr/bin/env bash

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

  # Compilar cardano-node y cabal/GHC/HLS
  nix develop --accept-flake-config  /root/cardano-node  -c bash -lc "
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

  # Calcular rutas finales de binarios
  NIX_BIN_DIR=$(dirname $(find /nix/store -name nix -type f -print -quit))
  CARDANO_CLI_DIR=$(dirname $(find /nix/store -name cardano-cli -type f -print -quit))
  CARDANO_NODE_DIR=$(dirname $(find / -name cardano-node -type f -print -quit))

  echo "export NIX_BIN_DIR=$NIX_BIN_DIR"       >> /root/.bashrc
  echo "export CARDANO_CLI=$CARDANO_CLI_DIR"  >> /root/.bashrc
  echo "export CARDANO_NODE=$CARDANO_NODE_DIR" >> /root/.bashrc
  echo "export PATH=\$PATH:$NIX_BIN_DIR:$CARDANO_CLI_DIR:$CARDANO_NODE_DIR" >> /root/.bashrc

  touch /root/.cardano_built
else
  echo "✔ Cardano Node ya compilado"
fi

exec bash
EOF

RUN dos2unix /usr/local/bin/build-cardano.sh && chmod +x /usr/local/bin/build-cardano.sh

# Alias para entrar en devShell
RUN echo 'alias dev="nix develop --accept-flake-config /root/cardano-node"' >> /root/.bashrc

RUN /usr/local/bin/build-cardano.sh 

RUN  HLS_BIN=$(find /nix/store -type f -name haskell-language-server -print -quit) && \
  echo "export HLS_BIN=$HLS_BIN" > /etc/profile.d/hls.sh && \
  chmod +x /etc/profile.d/hls.sh && \
  GHC_BIN=$(find /nix/store -type f -name ghc -print -quit) && \
  CABAL_BIN=$(find /nix/store -type f -name cabal -print -quit) && \
  echo "export GHC_BIN=$GHC_BIN"     >  /etc/profile.d/haskell.sh && \
  echo "export CABAL_BIN=$CABAL_BIN" >> /etc/profile.d/haskell.sh && \
  echo 'export PATH=$PATH:$(dirname $GHC_BIN):$(dirname $CABAL_BIN)' \
     >> /etc/profile.d/haskell.sh && \
  chmod +x /etc/profile.d/haskell.sh && \
  ln -sf "$GHC_BIN"   /usr/local/bin/ghc && \
  ln -sf "$CABAL_BIN" /usr/local/bin/cabal



# CMD final: arranca build al primer inicio y entra en bash con devShell
# CMD ["/bin/bash", "-lc", "/usr/local/bin/build-cardano.sh"]

CMD ["/bin/bash"]

