// RUN: %neal-swift

struct T {
  var key: String?
}
var p: KeyPath<T, String?> { return \T.key }
var q: KeyPath<T, Int?> { return \.key?.count }
var r: KeyPath<T, Int> { return \.key!.count }
var s: KeyPath<T, Int?> { return \[String: String?].["key"]?.count }

print(p)
print(q)
print(r)
print(s)
