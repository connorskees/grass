#![cfg_attr(feature = "nightly", feature(track_path))]

use std::{cell::RefCell, collections::HashSet, path::PathBuf};

use grass_compiler::StdFs;
use proc_macro::TokenStream;
#[cfg(not(feature = "nightly"))]
use quote::format_ident;
use syn::{parse_macro_input, LitStr};

use quote::__private::TokenStream as TokenStream2;

#[derive(Debug)]
struct FileTracker<'a> {
    files: RefCell<HashSet<PathBuf>>,
    fs: &'a dyn grass_compiler::Fs,
}

impl<'a> grass_compiler::Fs for FileTracker<'a> {
    fn is_dir(&self, path: &std::path::Path) -> bool {
        #[cfg(feature = "nightly")]
        if let Ok(p) = std::fs::canonicalize(path) {
            self.files.borrow_mut().insert(p);
        }

        self.fs.is_dir(path)
    }

    fn is_file(&self, path: &std::path::Path) -> bool {
        #[cfg(feature = "nightly")]
        if let Ok(p) = std::fs::canonicalize(path) {
            self.files.borrow_mut().insert(p);
        }

        self.fs.is_file(path)
    }

    fn read(&self, path: &std::path::Path) -> std::io::Result<Vec<u8>> {
        if let Ok(p) = std::fs::canonicalize(path) {
            self.files.borrow_mut().insert(p);
        }

        self.fs.read(path)
    }
}

#[cfg(not(feature = "nightly"))]
fn track_files(files: &HashSet<PathBuf>) -> TokenStream2 {
    let mut s: TokenStream2 = quote::quote!();

    for (idx, file) in files.iter().enumerate() {
        let ident = format_ident!("__VAR{}", idx);
        let file_name = file.to_string_lossy();
        s.extend::<TokenStream2>(quote::quote!(
            const #ident: &str = include_str!(#file_name);
        ));
    }

    s
}

#[cfg(feature = "nightly")]
fn track_files(files: &HashSet<PathBuf>) {
    for file in files {
        proc_macro::tracked_path::path(file.to_string_lossy());
    }
}

#[cfg(not(feature = "nightly"))]
fn finish(css: String, files: &HashSet<PathBuf>) -> TokenStream {
    let files = track_files(files);

    quote::quote!(
        {
            #files
            #css
        }
    )
    .into()
}

#[cfg(feature = "nightly")]
fn finish(css: String, files: &HashSet<PathBuf>) -> TokenStream {
    track_files(files);
    quote::quote!(#css).into()
}

#[proc_macro]
pub fn include_sass(item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as LitStr);

    let options = grass_compiler::Options::default();

    let fs = FileTracker {
        files: RefCell::new(HashSet::new()),
        fs: &StdFs,
    };

    let value = input.value();

    let css = match grass_compiler::from_path(
        value,
        &options
            .fs(&fs)
            .style(grass_compiler::OutputStyle::Compressed),
    ) {
        Ok(css) => css,
        Err(e) => {
            let err = syn::Error::new(input.span(), format!("Failed to compile Sass\n{}", e));
            return syn::Error::into_compile_error(err).into();
        }
    };

    let files = &*fs.files.borrow();

    finish(css, files)
}
