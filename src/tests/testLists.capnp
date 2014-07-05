# capnp-ocaml
#
# Copyright (c) 2013-2014, Paul Pelzl
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#  1. Redistributions of source code must retain the above copyright notice,
#     this list of conditions and the following disclaimer.
#
#  2. Redistributions in binary form must reproduce the above copyright
#     notice, this list of conditions and the following disclaimer in the
#     documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

@0xfc014463ebb6551b;

using Test = import "/capnp/test.capnp";

struct VoidList {
  a @0 : List(Void);
}

struct BoolList {
  a @0 : List(Bool);
}

struct Int8List {
  a @0 : List(Int8);
}

struct Int16List {
  a @0 : List(Int16);
}

struct Int32List {
  a @0 : List(Int32);
}

struct Int64List {
  a @0 : List(Int64);
}

struct UInt8List {
  a @0 : List(UInt8);
}

struct UInt16List {
  a @0 : List(UInt16);
}

struct UInt32List {
  a @0 : List(UInt32);
}

struct UInt64List {
  a @0 : List(UInt64);
}

struct Float32List {
  a @0 : List(Float32);
}

struct Float64List {
  a @0 : List(Float64);
}

struct TextList {
  a @0 : List(Text);
}

struct DataList {
  a @0 : List(Data);
}

struct TestNewVersionList {
  a @0 : List(Test.TestNewVersion);
}

struct TestOldVersionList {
  a @0 : List(Test.TestOldVersion);
}

