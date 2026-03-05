use crate::syntax::ast::Function;

const FNV_OFFSET_BASIS_64: u64 = 0xcbf29ce484222325;
const FNV_PRIME_64: u64 = 0x100000001b3;

fn fnv1a64(bytes: &[u8]) -> u64 {
    let mut hash = FNV_OFFSET_BASIS_64;
    for byte in bytes {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(FNV_PRIME_64);
    }
    hash
}

pub fn hash_str(input: &str) -> String {
    format!("{:016x}", fnv1a64(input.as_bytes()))
}

pub fn hash_function_ast(function: &Function) -> String {
    hash_str(&format!("{:?}", function))
}
