# clj-webgpu

WIP bindings for webgpu.

## Rationale

GPUs are much faster for some workloads. I want to be able to write programs that run on my mac as well as with nvidia gpus on linux. Even running the most basic compute shader with Vulkan is [really gnarly](https://github.com/mcleary/VulkanHpp-Compute-Sample/blob/master/main.cpp). However, webgpu is a new cross platform option that is gaining traction and support.

## Deps

```
com.phronemophobic/clj-webgpu {:git/url "https://github.com/phronmophobic/clj-webgpu" :git/sha "daa0179c51c2689c3fabaef650afa1719416aa7d"}
;; native dependencies
com.phronemophobic.cljonda/webgpu-native-darwin-aarch64 {:mvn/version "v0.19.4.1"}
com.phronemophobic.cljonda/webgpu-native-linux-x86-64 {:mvn/version "v0.19.4.1"}
com.phronemophobic.cljonda/webgpu-native-darwin-x86-64 {:mvn/version "v0.19.4.1"}
```

## Usage

See [examples](https://github.com/phronmophobic/clj-webgpu/tree/master/examples).

## Native Dependencies

In theory, these examples could run against any webgpu implemtation, but this example has only been tested with [webgpu-native](https://github.com/gfx-rs/wgpu-native), the implementation by Mozilla. The maven native binaries are just reuploads of the [releases](https://github.com/gfx-rs/wgpu-native/releases) provided by the wgpu-native github project. You can build or download the native dependencies yourself as long as the shared library is somewhere that JNA can find it. For example, in a folder specified with `:jvm-opts ["-Djna.library.path=/path/to/folder/"]`.

## Inspiration

- WebGPU is Not Just about the Web: https://youtu.be/qHrx41aOTUQ
- https://github.com/AnswerDotAI/gpu.cpp

## License

Copyright © 2024 Adrian

Distributed under the under Apache License v2.0.
