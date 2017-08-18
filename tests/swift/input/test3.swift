// RUN: %neal %args

import UIKit

// single line comment
/**
 * multiline
 * comment
**/
public protocol CollectionViewDataSource: UICollectionViewDataSource {
  func hello() -> Any
}
