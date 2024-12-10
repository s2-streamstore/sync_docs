# sync_docs

This proc macro allows us to inject documentation from prost generated rust file into our sdk types, mainly beneficial to avoid having to 
duplicate the documentation.

In `build.rs` of your repository, specify filename of the generated rust file.

```rs
println!("cargo:rustc-env=COMPILED_PROST_FILE=s2.v1alpha.rs");
```