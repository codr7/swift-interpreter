// swift-tools-version:5.9

import PackageDescription

let package = Package(
  name: "swiftr",

  products: [
    .executable(
      name: "swiftr",
      targets: ["swiftr"])
  ],

  dependencies: [
  ],
  
  targets: [
    .executableTarget(
      name: "swiftr",
      dependencies: []),
  ]
)
