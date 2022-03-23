// RUN: %neal-swift

final class Foo {
    func bar() {
        button.addTarget(self, action: #selector(baz), for: .touchUpInside)
    }
}
