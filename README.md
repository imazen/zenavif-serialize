# zenavif-serialize → moved to [imazen/zenavif](https://github.com/imazen/zenavif)

**This repository is archived.** On 2026-07-16 it was absorbed — full git
history included — into the [imazen/zenavif](https://github.com/imazen/zenavif)
cargo workspace, where development continues.

- **Code**: [`zenavif-serialize/`](https://github.com/imazen/zenavif/tree/main/zenavif-serialize)
  in imazen/zenavif. `git log -- zenavif-serialize/` there walks this
  repository's entire history.
- **Releases and tags**: imported crate-prefixed as
  [`zenavif-serialize-v*`](https://github.com/imazen/zenavif/tags); GitHub
  releases were recreated there with provenance notes.
- **Issues / PRs**: please file against
  [imazen/zenavif](https://github.com/imazen/zenavif/issues).
- **The crate is unaffected**: [crates.io](https://crates.io/crates/zenavif-serialize) /
  [docs.rs](https://docs.rs/zenavif-serialize) /
  [lib.rs](https://lib.rs/crates/zenavif-serialize) continue as before;
  versions from 0.2.0 on publish from the workspace.

zenavif-serialize is a pure-Rust AVIF container serializer (muxer) — it wraps
already-compressed AV1 bitstreams into MPEG/HEIF/MIAF/ISO-BMFF boxes for still
images, animations, grids, and gain maps. `#![forbid(unsafe_code)]`.
BSD-3-Clause, as before.
