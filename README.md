
# QMP

OCaml implementation of the Qemu Monitor Protocol (QMP)

## Building

Install the dependencies using OPAM:

```sh
opam install yojson cmdliner dune ounit2
```

Build the library, tests, and CLI:

```sh
dune build
dune runtest
dune install
```

## Building from Sources using Opam

```sh
opam pin add .
```

## CLI

Try the CLI:

```sh
qmp-cli
```

Before installation the binary is located in
`_build/default/cli/cli.exe`. It will get installed under the correct
name `qmp-cli` by `dune install`
