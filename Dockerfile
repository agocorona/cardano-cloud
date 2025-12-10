# compile all the cardano-node stuff from ubuntu to make it ready for VSCode development. 

FROM ubuntu

# Variables de entorno para el Locale UTF-8 (Crítico para Haskell/GHC)
ENV LANG en_US.UTF-8
ENV LC_ALL en_US.UTF-8

RUN apt-get update && apt-get install -y curl bzip2 xz-utils git bash locales && \
    # Configuración de LOCALE UTF-8 para evitar errores de codificación
    locale-gen en_US.UTF-8 && \
    # Creamos /nix ANTES de la instalación.
    mkdir -p /nix /etc/nix && \
    chmod a+rwx /nix

RUN adduser user --home /home/user --disabled-password --gecos "" --shell /bin/bash

USER user
ENV USER user
WORKDIR /home/user



# 3. Instalación de Nix Multi-User (Más fiable para crear /nix/store)
# Usamos el script moderno, sin argumentos de single-user.
RUN curl -L https://nixos.org/nix/install | sh

# 4. Configuración del Perfil (Activación y Creación del Symlink)
# Usamos la ruta más genérica al binario de nix-env, que existe después de una instalación Multi-User exitosa.
# Esto crea el symlink /root/.nix-profile/bin/nix
#RUN /bin/bash -c "\
#    /nix/var/nix/profiles/default/bin/nix-channel --update || true; \
#    /nix/var/nix/profiles/default/bin/nix-env -iA nixpkgs.nix; \
#"

USER root
ENV USER root

# 5. Establecer el PATH de Nix de forma permanente
ENV PATH="/root/.nix-profile/bin:$PATH"

# 6. Clonar el repositorio
RUN git clone https://github.com/input-output-hk/cardano-node.git /root/cardano-node 
WORKDIR /root/cardano-node

# 2. Configuración global de Nix (build-users-group = es esencial)
RUN printf "experimental-features = nix-command flakes\n\
build-users-group =\n\
substituters = https://cache.iog.io https://cache.nixos.org\n\
trusted-public-keys = cache.iog.io-1:HPVKrDdmoQy8VHxFEkY0YtVvzGwMHgO4pY1GSwpMq7g= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJcM1QEOQJYv9j7hWc=\n\
fallback = true\n" > /etc/nix/nix.conf


# 7. Ejecutar la compilación (Detección dinámica corregida)
RUN /bin/bash -c "\
    # 1. Busca el binario 'nix' y obtiene su directorio contenedor.
    # Usamos '-name nix' para buscar el ejecutable principal.
    NIX_BIN_DIR=$(dirname $(find /nix/store -name nix -type f -print -quit)); \
    \
    # 2. ESCRIBIR EN .bashrc (Persistencia interactiva).
    echo export PATH=\$PATH:$NIX_BIN_DIR >> .bashrc; \
    \
    # 3. EXPORTAR EN EL SHELL ACTUAL (Efectividad inmediata).
    export PATH=\"\$NIX_BIN_DIR:\$PATH\"; \
    \
    echo 'Nix binario detectado y configurado en PATH y .bashrc: $NIX_BIN_DIR'; \
    \
    # 4. Ejecutar el comando final.
    nix develop --accept-flake-config .#devShells.x86_64-linux.default --command bash -c '\
        cabal update && \
        cabal build all \
    '; \
"

RUN mkdir -p /usr/local/bin

RUN printf '#!/bin/bash\n\
exec nix develop /root/cardano-node --command cardano-node "$@"\n' \
  > /usr/local/bin/cardano-node && chmod +x /usr/local/bin/cardano-node

RUN printf '#!/bin/bash\n\
exec nix develop /root/cardano-node --command cardano-cli "$@"\n' \
  > /usr/local/bin/cardano-cli && chmod +x /usr/local/bin/cardano-cli
