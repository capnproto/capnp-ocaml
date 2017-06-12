@0xeb2643c5955c2d28;

struct Foo {
  bar @0 :Bar;
  x @1 :Int32;
}

struct Bar {
  foo @0 :Foo;
}
