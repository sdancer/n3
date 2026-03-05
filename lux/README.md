# Lux

Minimalist compiler targeting Erlang/BEAM with content-addressed function modules.

## License

MIT. See [LICENSE](./LICENSE).

## Run Example

This compiles one `.core` and one `.beam` per function (module names are hashes), and writes metadata for introspection.

```bash
cd /home/sdancer/lux/lux
cargo run -- examples/fib.lux
```

Use metadata to discover the entry module hash:

```bash
cat fib.meta.json
```

Run the compiled entry function (replace module hash if different):

```bash
erl -noshell -pa /home/sdancer/lux/lux -eval "io:format(\"~p~n\", ['e8a56de9f0f836e0':apply()]), halt()."
```

Expected output for `examples/fib.lux`:

```text
55
```
