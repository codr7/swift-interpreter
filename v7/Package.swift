// swift-tools-version:5.9

import PackageDescription

let package = Package(
  name: "swinter",

  products: [
    .executable(
      name: "swinter",
      targets: ["swinter"])
  ],

  dependencies: [
  ],
  
  targets: [
    .executableTarget(
      name: "swinter",
      dependencies: []),
  ]
)
