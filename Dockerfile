# Base Ubuntu image
FROM ubuntu

# Set UTF-8 locale environment variables (required for Haskell/GHC)
ENV LANG en_US.UTF-8
ENV LC_ALL en_US.UTF-8

# Install required packages, configure locale, and prepare /nix directory
RUN apt-get update && apt-get install -y curl bzip2 xz-utils unzip git bash locales && \
    # Generate UTF-8 locale
    locale-gen en_US.UTF-8 && \
    # Create /nix before installation
    mkdir -p /nix /etc/nix && \
    chmod a+rwx /nix

# Create non-root user "user"
RUN adduser user --home /home/user --disabled-password --gecos "" --shell /bin/bash

# Switch to the unprivileged user
USER user
ENV USER user
WORKDIR /home/user

# Install Nix in multi-user mode (recommended)
RUN curl -L https://nixos.org/nix/install | sh

# Switch back to root user
USER root
ENV USER root

# Add Nix profile to PATH
ENV PATH="/root/.nix-profile/bin:$PATH"

# Clone the Cardano Node repository
RUN git clone https://github.com/input-output-hk/cardano-node.git /root/cardano-node
WORKDIR /root/cardano-node

# Global Nix configuration (enables flakes + no build-users-group)
RUN printf "experimental-features = nix-command flakes\n\
build-users-group =\n\
substituters = https://cache.iog.io https://cache.nixos.org\n\
trusted-public-keys = cache.iog.io-1:HPVKrDdmoQy8VHxFEkY0YtVvzGwMHgO4pY1GSwpMq7g= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJcM1QEOQJYv9j7hWc=\n\
fallback = true\n" > /etc/nix/nix.conf

RUN /bin/bash -c "\
    # Detect the Nix binary path dynamically
    NIX_BIN_DIR=$(dirname $(find /nix/store -name nix -type f -print -quit)); \
    \
    # Append path to .bashrc for interactive sessions
    echo export PATH=\$PATH:$NIX_BIN_DIR >> $HOME/.bashrc; \
    \
    # Export path for current shell
    export PATH=\"\$NIX_BIN_DIR:\$PATH\"; \
    \
    echo 'Detected Nix binary directory: $NIX_BIN_DIR'; \
    \
    # Build all Cardano components using cabal inside Nix devshell
    nix develop --accept-flake-config .#devShells.x86_64-linux.default --command bash -c '\
        cabal update && \
        cabal build all \
    '; \
"

# Install Ogmios (download + extract + find binary)
ARG OGMIO_VERSION=6.14.0
ARG OGMIO_ARCH=x86_64-linux
RUN curl -L "https://github.com/CardanoSolutions/ogmios/releases/download/v${OGMIO_VERSION}/ogmios-v${OGMIO_VERSION}-${OGMIO_ARCH}.zip" \
    -o /tmp/ogmios.zip && \
    mkdir -p /tmp/ogmios && \
    unzip /tmp/ogmios.zip -d /tmp/ogmios && \
    find /tmp/ogmios -type f -name ogmios -exec mv {} /usr/local/bin/ogmios \; && \
    chmod +x /usr/local/bin/ogmios && \
    rm -rf /tmp/ogmios /tmp/ogmios.zip

# Calcular rutas reales de los binarios durante el build
RUN NIX_BIN_DIR=$(dirname $(find /nix/store -name nix -type f -print -quit)) && \
    CARDANO_CLI=$(dirname $(find /nix/store -name cardano-cli -type f -print -quit)) && \
    CARDANO_NODE=$(dirname $(find / -name cardano-node -type f -print -quit)) && \
    echo "export NIX_BIN_DIR=$NIX_BIN_DIR"       >> /root/.bashrc && \
    echo "export CARDANO_CLI=$CARDANO_CLI"       >> /root/.bashrc && \
    echo "export CARDANO_NODE=$CARDANO_NODE"     >> /root/.bashrc && \
    echo "export PATH=\$PATH:$NIX_BIN_DIR:$CARDANO_CLI:$CARDANO_NODE" \
        >> /root/.bashrc

# Create alias "bash" that enters the cardano-node devShell
RUN printf '\n\
alias dev="nix develop --accept-flake-config /root/cardano-node"\n' \
    >> /root/.bashrc

# Keep container alive for development
CMD ["sleep", "infinity"]
