// RUN: %neal-swift

// Await keyword
let photoNames = try listPhotos(inGallery: "Summer Vacation")
let photoNames = try await listPhotos(inGallery: "Summer Vacation")
let photoNames = await listPhotos(inGallery: "Summer Vacation")
await listPhotos(inGallery: "Summer Vacation")

func listPhotos(inGallery name: String) async throws -> [String] {
    try await Task.sleep(nanoseconds: 2 * 1_000_000_000)
    return ["IMG001", "IMG99", "IMG0404"]
}

// Asynchronous Sequences
for try await line in handle.bytes.lines {
    print(line)
}

for await line in handle.bytes.lines {
    print(line)
}