# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

<!-- ## [Unreleased] -->
## [0.8.1] - 2023-09-27
### Added
- Added `FlagOrValue::{is_none, is_flag, is_value, into_value, as_value}`

## [0.8.0] - 2023-09-18
### Changed
- Renamed `Attribute` to `FromAttr` to not conflict with `syn::Attribute`.
  The old path is still exported (#[doc(hidden)]). Existing usages should not break.

## [0.7.1] - 2023-09-17
### Fixed
- `FlagOrValue` only supported some `syn` types.

## [0.7.0] - 2023-09-17
- Updated dependencies

### Added
- Added `FlagOrValue` to support a value both as a Flag and a Value

### Changed
- `bool` now allows specifying a flag multiple times.

### Fixed
- Specifying a param multiple times in the same attribute was ignored.

## [0.6.1] - 2023-05-21
- Updated dependencies

## [0.6.0] - 2023-03-20
### Changed
- Updated `syn` to v2

## [0.5.0] - 2023-03-02
### Added
- `IdentValue` to keep hold of both value and the source ident

### Changed
- **Breaking Change**: `ConvertParsed::aggregate()` now takes/returns
  `IdentValue`
- Improved span for conflicting values

## [0.4.0] - 2023-03-02
### Changed
- **Breaking Change**: Moved some syn types behind feature `full`
- **Breaking Change**: Refactored attributes
- Use [interpolator](https://docs.rs/interpolator) for error messages
- **Breaking Change**: Compile time verify if ident is given through helper
  trait

[unreleased]: https://github.com/ModProg/attribute-derive/compare/v0.8.1...HEAD
[0.8.1]: https://github.com/ModProg/attribute-derive/compare/v0.8.0...v0.8.1
[0.8.0]: https://github.com/ModProg/attribute-derive/compare/v0.7.1...v0.8.0
[0.7.1]: https://github.com/ModProg/attribute-derive/compare/v0.7.0...v0.7.1
[0.7.0]: https://github.com/ModProg/attribute-derive/compare/v0.6.1...v0.7.0
[0.6.1]: https://github.com/ModProg/attribute-derive/compare/v0.6.0...v0.6.1
[0.6.0]: https://github.com/ModProg/attribute-derive/compare/v0.5.0...v0.6.0
[0.5.0]: https://github.com/ModProg/attribute-derive/compare/v0.3.1...v0.5.0
[0.4.0]: https://github.com/ModProg/attribute-derive/compare/v0.3.1...v0.4.0
