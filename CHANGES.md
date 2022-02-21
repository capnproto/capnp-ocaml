# v3.5.0

New features:

* Add Codecs.serialize_{fold,iter}_copyless (@Cjen1, #71).
  Avoids a copy when writing out fragments.

Documentation:

* Add example of how to use the library (@rottened23, #62).

* Extract LICENSE and identify as BSD-2-Clause (@tmcgilchrist, #80).

Build system:

* Update to dune 2 (@talex5, #77).

* Dune should not be a build dependency (@talex5, #67).

* Replace `Uint32.of_int` in unit-tests with `of_string` (@talex5, #73).
  Fixed tests on 32-bit systems.

* Remove Travis CI (@talex5, #74). Replaced by ocaml-ci.

* Switch from "capnpc" to "capnp compile" (@talex5, #75).

* Benchmarks: remove incorrect test for 64-bit on Windows (@dra27, #72).

# v3.4.0

* Upgrade from uint to stdint (@talex5, #64). From uint 2.0.1, these are
  compatible. Projects using the schema compiler should either upgrade to
  stdint themselves, or add a lower bound of `uint >= 2.0.1`. Otherwise,
  the build may fail because of a missing dependency on stdint.

* Require `base >= 0.11` for `Base.Queue` (@talex5, #61).

* Fix compilation on OCaml 4.08 (@talex5, #63).

* Fix generated files when one interface imports another (@talex5, #65,
  reported by TealG).

# v3.3.0

This release is mostly about reducing the number of dependencies.
The only API change is that `write_message_to_file_robust` has gone.

* Remove `extunix` dependency (#57).
  `write_message_to_file_robust` was its only user, and wasn't used by anything.
  Using `fsync` to ensure a file is written to disk is a general function that
  should be handled by the user of capnp where needed.
  Removing `extunix` also removes the indirect dependency on `camlp4`,
  which in turn allows `capnp` to build with OCaml 4.08 (and to build faster on all versions).

* Remove `Pervasives` qualifier (#57).
  This is needed to support OCaml 4.08 without warnings.

* Remove dependency on `Core` from benchmarks (#55).
  This was the only remaining use of the library.

* Replace uses of `Core_kernel` with plain `Base` and `Stdio` in the compiler (#56)
  and tests (#59).
  This greatly reduces the number of libraries you need to install to install capnp.

* Update tests for the current quickcheck API (#54).

* Switch from jbuilder to dune (#54).

* Require OCaml >= 4.03 (#54).
  This allows us to drop some complexity from the jbuilder files,
  allowing them to be upgraded automatically by dune.

* Upgrade to opam 2 format (#54).

* Convert the changelog to markdown (#58).
  This allows it to be used with dune-release.

With these changes, the following 49 libraries that were needed to install
capnp-ocaml 3.2.1 are no longer required: `base_bigstring`, `base_quickcheck`,
`bin_prot`, `camlp4`, `core_kernel`, `extunix`, `fieldslib`,
`jane-street-headers`, `jst-config`, `num`, `ocaml-compiler-libs`,
`ocaml-migrate-parsetree`, `octavius`, `parsexp`, `ppx_assert`, `ppx_base`,
`ppx_bench`, `ppx_bin_prot`, `ppx_compare`, `ppx_custom_printf`,
`ppx_derivers`, `ppx_enumerate`, `ppx_expect`, `ppx_fail`, `ppx_fields_conv`,
`ppx_hash`, `ppx_here`, `ppx_inline_test`, `ppx_jane`, `ppx_js_style`,
`ppx_let`, `ppxlib`, `ppx_module_timer`, `ppx_optcomp`, `ppx_optional`,
`ppx_pipebang`, `ppx_sexp_conv`, `ppx_sexp_message`, `ppx_sexp_value`,
`ppx_stable`, `ppx_typerep_conv`, `ppx_variants_conv`, `re`, `seq`, `sexplib`,
`splittable_random`, `time_now`, `typerep` and `variantslib`.

# v3.2.1

## Other changes

* Update for API changes in Base and Core\_kernel (#52).

# v3.2.0

## Backwards-incompatible changes

* The `capnp` opam package no longer depends on the C++ compiler (#47).
  If your project compiles schema files, you should add
  `"conf-capnproto" {build}` to your opam dependencies.

## Other changes

* Add support for OCaml 4.06 (#45).

* Update for API changes in Base and Core\_kernel (#44).

# v3.1.0

## Backwards-incompatible changes

* The Unix-specific `IO` module has been moved from `Capnp.IO` to `Capnp_unix.IO`,
  and is now part of the new `capnp.unix` ocamlfind library.
  Both `capnp` and `capnp.unix` ocamlfind libraries are provided by the `capnp`
  opam package.

## New features

* The core `capnp` library no longer depends on `Core_kernel` or `Unix`.
  This makes binaries using the library several MB smaller and allows
  capnp to be used in [Mirage unikernels](https://mirage.io/).

# v3.0.0

## Backwards-incompatible changes

* The `Builder.X.reader_t` and `Reader.X.builder_t` types have gone
  (to avoid confusion with the generic `reader_t` and `builder_t` types).
  Use `X.struct_t reader_t` and `X.struct_t builder_t` instead.
  `Builder.X.t` is still defined (as an alias for `t builder_t`), and similarly
  for `Reader.X.t`.

* `StructStorage.t` now takes an extra type parameter. This is a phantom type,
  used to indicate what kind of struct it represents. This extra parameter
  has also been added to `Object.t`.

* In the generated files, the unique types names (e.g.
  `reader_t_Foo_14133145859926553711`) have gone.
  If you need to refer to these directly for some reason, use the replacement
  polymorphic type instead (e.g. `Foo_c42305476bb4746f reader_t`).

## New features

* RPC support (#35 and #36).
  The compiler now generates readers and builders for parameters and results,
  interface and method IDs, and a dispatch method for services.
  There is a new `MakeRPC` functor in the generated files that takes an
  extended version of `MessageSig`. The original `Make` is still provided - it
  is now a convenience wrapper around `MakeRPC` that passes some dummy functions.
  An RPC provider is available at <https://github.com/mirage/capnp-rpc>.

* Capability references can now be attached to messages.
  The getters and setters that previously took `Uint32.t` indexes can now take
  capabilities, as defined by an RPC provider library.
  The default `RPC.None` provider exposes the raw descriptor index, as before.

* Better AnyPointer support (#11). The new `of_pointer` and `init_pointer` functions
  can be used to read and add structs of any type to AnyPointer fields.
  The new `pointer_set_reader` can be used to set a pointer by copying from a reader.

* Replace camlp4 includes with flambda inlining (#23).
  The `-inc.ml` files that were previously copied into each generated file are
  now regular modules in the library, making them available to other code too.
  This makes the generated files much smaller. There is no performance penalty
  (when using flambda).

* Port to jbuilder (#24).
  Also, the required schema files are now included in the repository rather than being
  downloaded by the build scripts, which was fragile.

* Add support for cycles (#29).
  Before, the compiler would abort if a schema file contained cycles, but this
  is very common (especially with interfaces). It now generates forward
  references where needed.

* Add parameterised `reader_t` and `builder_t` types (#32 and #33).
  Instead of providing `Reader.Foo.t` and `Builder.Foo.t` as abstract types,
  provide a single `Foo.struct_t` and use `struct_t reader_t` and
  `struct_t builder_t` for its reader and builder.
  This makes it possible to define generic functions that work on all readers
  or builders, without having to generate a separate function for each one.
  The types are shared between files, so you can write generic code to work
  with schema files you haven't seen (needed for the RPC system).

* Use polymorphic variants for phantom types (#34).
  Instead of declaring a load of unique type names at the start of the file,
  use polymorphic variants. These don't need to be declared before use.
  The node ID is now formatted as hex, to make it shorter and to match the
  format used in the schema files.
  There are new generic `reader_of_builder` and `message_of_builder` functions
  in `StructStorage`.

## Other changes

* All uses of `Obj.magic` have been removed from the generated code.

* Update to latest core\_kernel (#26).
  Avoids conflict with newer sexplib.

* Fix some compiler warnings (#28). Jbuilder turns on more warnings by default.

* Add missing test dependency on ounit (#30).

* Fix code generation bug in 2.1.1 (#10).
  A missing space prevented the generated files from compiling.
  Added Travis CI tests to detect this kind of thing automatically.

# v2.1.1

* Fix `Not_found` exception when a capnp import is not referenced within the schema file.

* Omit references to capnp imports which are not used within the generated code.  In
  particular, importing `"/capnp/c++.capnp"` no longer generates a reference to an OCaml
  module with the confusing name of `C2b2b`, and there should no longer be a need to
  separately compile that imported schema file.

# v2.1.0

* Depend on Core\_kernel rather than Core. Thanks to Matthew Maurer for
  driving this change.

* Fix compatibility issue when building against Core 112.35.00.

# v2.0.1

* Invoke OCaml scripts using `-I $OCAML_TOPLEVEL_PATH`, to avoid missing
  `topfind`.

# v2.0.0

## Backwards-incompatible changes

* Module `Codecs`: change API to accept the more natural `BytesMessage.Message.t`
  instead of `Bytes.t list`.

* Module `Codecs`: change API to accept a `compression` specifier, instead of using
  separate functions and separate types for compressed and uncompressed streams.

* Module `Message`: `to_storage` now returns a list of (storage, length) pairs.

## Other changes

* Module `Codecs`: fix incorrect encoding of framing headers (for example,
  as generated by `serialize`).

* Module `Codecs`: fix infinite loop in `PackedStream` decoding.

* Module `Codecs`: reduce serialized message sizes, by omitting unused allocation
  regions from the serialized messages.

* Instantiate `BytesMessage = Message.Make(BytesStorage)`, so the user doesn't always
  need to. (Implementation is furthermore defunctorized for improved performance.)

* New module `IO`: functions for moving message data through various types of I/O
  channels.

* Compiler now says something about files it created.

* Corrected compilation errors when using 4.02 `-safe-string`.

* Significant performance improvements across the board.

# v1.0.1

* Avoid use of GNU `install` features in `omake install`.  This corrects installation
  problems on OS X and (most likely) other BSDs.

# v1.0.0

* Initial release.
