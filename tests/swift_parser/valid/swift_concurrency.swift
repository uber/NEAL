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

// Parallel execution
async let firstPhoto = downloadPhoto(named: photoNames[0])
async let secondPhoto = downloadPhoto(named: photoNames[1])
async let thirdPhoto = downloadPhoto(named: photoNames[2])

let photos = await [firstPhoto, secondPhoto, thirdPhoto]

// Task Group
await withTaskGroup(of: Data.self) { taskGroup in
    let photoNames = await listPhotos(inGallery: "Summer Vacation")
    for name in photoNames {
        taskGroup.addTask { await downloadPhoto(named: name) }
    }
}

let handle = Task {
    return await add(newPhoto, toGalleryNamed: "Spring Adventures")
}
let result = await handle.value

// Actor
actor TemperatureLogger {
    let label: String
    var measurements: [Int]
    private(set) var max: Int

    init(label: String, measurement: Int) {
        self.label = label
        self.measurements = [measurement]
        self.max = measurement
    }
}