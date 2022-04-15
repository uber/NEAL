// RUN: %not %neal %args | %check

async function test(array) {
  for (let f of array) {

    // CHECK: error: Do not use `await` inside loops
    await f()
  }
}
