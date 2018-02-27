// RUN: %neal-swift

final class Foo {
    func bar() {
      print (
        """
        Shouldn't be able to update a pickup location if there is no
        current ride, or the map isn't fully initialized
        """
      )

      print (
        """
        Shouldn't \("foo") able to update a pickup location if there is no
        current ride, or the map isn't fully initialized
        """
      )
    }
}

Foo().bar()
