# Sessynth
Synthesizer for Session-Typed Programs

## Running Stuff

1. `dune build`
2. `dune exec ./sessint/bin/main.exe sessint/test/intqueue.sessint true false`
    - Enabling debug logs: `SESSYNTH_DEBUG=1 dune exec ./sessint/bin/main.exe sessint/test/intqueue.sessint true false`