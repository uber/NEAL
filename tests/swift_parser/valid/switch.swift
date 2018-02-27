// RUN: %neal-swift

final class Foo {
    func bar(return default: String) {
        let `case` = "foo"
        switch `case` {
        case "foo":
            return
        default:
            break
        }
    }
}
