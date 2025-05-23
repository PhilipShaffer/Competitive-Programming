FROM ocaml/opam:ubuntu-ocaml-5.2

# Install system dependencies
RUN sudo apt-get update && \
    sudo apt-get install -y \
    cmake \
    libzstd-dev \
    llvm-18-dev \
    clang \
    zlib1g-dev && \
    sudo apt-get clean && \
    sudo rm -rf /var/lib/apt/lists/*

# Install OCaml LSP server
RUN opam install -y ocaml-lsp-server ocamlformat

# Copy opam file and install dependencies
WORKDIR /workspace
COPY Competitive_Programming.opam .
RUN opam install . --deps-only -y