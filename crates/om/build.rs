fn main() {
    println!("cargo:rustc-link-arg-tests=/EXPORT:MdCallBack12");
    println!("cargo:rustc-link-arg-tests=/EXPORT:MdCallBack");
}
