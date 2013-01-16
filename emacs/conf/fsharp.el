;; Configuration for fsharp-mode

(autoload 'fsharp-mode "fsharp" "Major mode for editing F# code." t)
(autoload 'run-fsharp "inf-fsharp" "Run an inferior F# process." t)

(setq inferior-fsharp-program "/Library/Frameworks/Mono.framework/Versions/3.0.3/bin/fsharpi")
(setq fsharp-compiler "/Library/Frameworks/Mono.framework/Versions/3.0.3/bin/fsharpc")

