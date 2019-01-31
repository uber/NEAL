// RUN: %neal-swift

final class Foo {
    @available(*, unavailable, message: "Unavailable, use baz()")
    func bar() {
    }
}
