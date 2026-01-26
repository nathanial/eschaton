import Lake
open Lake DSL

package «eschaton» where
  leanOptions := #[
    ⟨`autoImplicit, false⟩
  ]

-- Local import for development
require afferent from "../../graphics/afferent"
require linalg from "../../math/linalg"

@[default_target]
lean_lib «Eschaton» where
  roots := #[`Eschaton]

-- Link args for Metal/macOS (inherited from afferent's native code)
def commonLinkArgs : Array String := #[
  "-framework", "Metal",
  "-framework", "Cocoa",
  "-framework", "QuartzCore",
  "-framework", "Foundation",
  "-framework", "Security",
  "-framework", "SystemConfiguration",
  "-lobjc",
  "-L/opt/homebrew/lib",
  "-L/usr/local/lib",
  "-lfreetype",
  "-lassimp",
  "-lz",
  "-lc++",
  "-lcurl"
]

lean_exe eschaton where
  root := `Main
  moreLinkArgs := commonLinkArgs

lean_exe tests where
  root := `Tests.Main
  moreLinkArgs := commonLinkArgs

@[test_driver]
script test do
  let result ← IO.Process.run {
    cmd := ".lake/build/bin/tests"
    args := #[]
  }
  IO.println result
  return 0
