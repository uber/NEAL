

func main() {
  let value = convertToInteger(0.123)
    DispatchQueue.main.async {
      let view = UIView()
      view.backgroundColor = .red
  }
}


func convertToInteger(x: Double) -> Int {
  return x as! Int
}
