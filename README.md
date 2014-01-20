# capnp-ocaml

This is an [OCaml](http://ocaml.org/) code generator plugin for the [Cap'n Proto](http://kentonv.github.io/capnproto/) serialization framework.

This code is **very incomplete** and the API is **not stable**. The generated code provides a read-only interface which is good enough to parse the most common types of messages.

## Design Notes

The generated code follows the general Cap'n Proto convention of using the serialized message format as the in-memory data structure representation. This enables the "infinitely faster" performance characteristic, but it also means that the generated code is not directly compatible with OCaml records, arrays, etc.

The generated code is functorized over the underlying message format. This enables client code to operate equally well on messages stored as--for example--OCaml strings or OCaml Bigarrays (perhaps most interesting for applications involving `mmap()`).

## Generated Code

The Cap'n Proto types are mapped to OCaml types as follows:

<table>
  <tr><td>Cap'n Proto Type</td><td>OCaml Type</td></tr>
  <tr><td><code>Void</code></td><td><code>unit</code></td></tr>
  <tr><td><code>Bool</code></td><td><code>bool</code></td></tr>
  <tr><td><code>Int8</code></td><td><code>int</code></td></tr>
  <tr><td><code>Int16</code></td><td><code>int</code></td></tr>
  <tr><td><code>Int32</code></td><td><code>int32</code></td></tr>
  <tr><td><code>Int64</code></td><td><code>int64</code></td></tr>
  <tr><td><code>Uint8</code></td><td><code>int</code></td></tr>
  <tr><td><code>Uint16</code></td><td><code>int</code></td></tr>
  <tr><td><code>Uint32</code></td><td><code>Uint32.t</code>
    (from <a href="https://github.com/andrenth/ocaml-uint"><code>uint</code></a>
    library)</td></tr>
  <tr><td><code>Uint64</code></td><td><code>Uint64.t</code>
    (from <a href="https://github.com/andrenth/ocaml-uint"><code>uint</code></a>
    library)</td></tr>
  <tr><td><code>Float32</code></td><td><code>float</code></td></tr>
  <tr><td><code>Float64</code></td><td><code>float</code></td></tr>
  <tr><td><code>Text</code></td><td><code>string</code></td></tr>
  <tr><td><code>Data</code></td><td><code>string</code></td></tr>
  <tr><td><code>List&lt;T&gt;</code></td><td><code>(T, 'a) Runtime.Array.t</code></td></tr>
</table>

The `Runtime.Array` module is a work-in-progress which will be roughly API-compatible with OCaml arrays and will provide similar performance characteristics.

A Cap'n Proto enum maps to an OCaml variant.  To enable forward compatibility, deserialized enums with an undefined value map to the variant constructor `Undefined\_ of int`.

A Cap'n Proto struct maps to an OCaml module.  Accessor functions for the struct fields are emitted within the associated module.  Due to the mismatch between OCaml types and Cap'n Proto types, additional accessors are emitted in order to automate type conversion:

```ocaml
module Foo : sig
  type t

  (* Accessors for struct field "bar" with type Uint32 *)

  val bar_get : t -> Uint32.t
  (** [bar_get foo] retrieves the value of field "bar" stored within struct [foo]. *)

  val bar_get_int_exn : t -> int
  (** [bar_get_int_exn foo] retrieves the value of field "bar" stored within
      struct [foo], coercing the value to [int].

      @raise [Out_of_range "bar_get_int_exn"] if the value is out of range for [int] *)

  (* (Mutable API yet implemented...) *)

  val bar_set : t -> Uint32.t -> unit
  (** [bar_set foo] sets the value of field "bar" stored within struct [foo]. *)

  val bar_set_int_exn : t -> int -> unit
  (** [bar_set_int_exn foo] sets the value of field "bar" stored within struct [foo],
      coercing from an [int] argument.

      @raise [Invalid_argument "bar_set_int_exn"] if the value is out of range for
      Uint32.t *)

  (* [snip] *)
end
```

A Cap'n Proto union maps to an OCaml variant in the way one would expect.  Once again, deserialized union discriminants with an undefined value map to the variant constructor `Undefined\_ of int`.

## Compiling

You'll need OCaml 4.01 and OCaml packages `core`, `uint`, `ocplib-endian`, and `res`.  All of these are available from [OPAM](http://opam.ocaml.org).

Compile using `ocaml setup.ml -build` .

## Installation

Sorry, this code is too unfinished for installation as a library.

## Contact

pelzlpj at gmail dot com

## License

Copyright (c) 2013-2014, Paul Pelzl
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of
   conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice, this list of
   conditions and the following disclaimer in the documentation and/or other materials
   provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

