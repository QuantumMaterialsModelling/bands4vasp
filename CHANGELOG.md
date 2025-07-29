# Changelog
All notable changes to this project will be documented in this file.
The format is based on [Keep a Changelog], and this project adheres to [Semantic Versioning].

## [0.5] - 2025-07-29
### Fixed
- Fixed a bug in the `--pre-` logic: sampling now works on arbitrary planes (not only special ones).

### Added
- Preview function for the `--pre-` logic. It is interactive when possible and falls back to a static preview in headless environments.
- New INPAR parameter `PREPREVIEW` to control preview behavior.
- New INPAR parameter `FSURTICS` to independently toggle Fermi‑surface tics on/off.

### Changed
- Corner edge vectors of the Fermi surface can now be output correctly in **Cartesian** or **reciprocal** coordinates via `FSUREDGEVEC` wich replaced `FSURCART`.
- Default output format set to **PNG**, and **pngcairo** is now the default gnuplot terminal.
- Fermi‑surface spectral function revised for improved results.

### Documentation
- Expanded documentation and tutorial to cover the new parameters and workflows.

