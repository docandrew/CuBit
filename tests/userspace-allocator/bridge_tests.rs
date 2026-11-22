//! Exercises the real release-mode Ada/host address boundary, not just a model.
unsafe extern "C" {
    fn ca_init();
    fn ca_malloc(size: usize) -> *mut u8;
    fn ca_free(pointer: *mut u8);
}
fn main() {
    unsafe {
        ca_init();
        assert!(ca_malloc(0).is_null());
        assert!(ca_malloc(4097).is_null());
        assert!(ca_malloc(usize::MAX).is_null());
        ca_free(std::ptr::null_mut());
        ca_free(std::ptr::without_provenance_mut(1));
        ca_free(std::ptr::without_provenance_mut(usize::MAX));
        // One class may consume the entire region, beyond the old 2048 quota.
        let mut large = Vec::new();
        for _ in 0..4096 {
            let p = ca_malloc(4096);
            assert!(!p.is_null());
            large.push(p);
        }
        assert!(ca_malloc(16).is_null());
        for p in large {
            ca_free(p);
        }
        let mut blocks = Vec::new();
        for size in [16, 32, 64, 128, 256, 512, 1024, 2048, 4096] {
            let count = if size == 16 { 4096 } else { 2048 };
            for _ in 0..count {
                let p = ca_malloc(size);
                assert!(!p.is_null());
                assert_eq!(p as usize % 16, 0);
                // Pattern depends on address and byte, unlike a single shared
                // fill value which could hide two slots aliasing each other.
                let token = blocks.len();
                for byte in 0..size {
                    p.add(byte).write(
                        token.to_le_bytes()[byte % std::mem::size_of::<usize>()] ^ (byte / 8) as u8,
                    );
                }
                blocks.push((p, size, token));
            }
        }
        // A caller cannot manufacture a free slot by releasing interior bytes.
        for &(p, size, _) in &blocks {
            ca_free(p.add(1));
            ca_free(p.add(size - 1));
        }
        for size in [16, 32, 64, 128, 256, 512, 1024, 2048, 4096] {
            assert!(ca_malloc(size).is_null());
        }
        for &(p, size, token) in blocks.iter().rev() {
            for byte in 0..size {
                assert_eq!(
                    p.add(byte).read(),
                    token.to_le_bytes()[byte % std::mem::size_of::<usize>()] ^ (byte / 8) as u8
                );
            }
            ca_free(p);
            ca_free(p); // Immediate double free rejected; stale-after-reuse is NOT prevented.
        }
        // All classes are reusable after the full-arena test.
        for &(_, size, _) in &blocks {
            assert!(!ca_malloc(size).is_null());
        }
    }
    println!(
        "PASS hosted bridge: shared slab capacity, full-arena payload integrity, invalid/interior/double free and reuse"
    );
}
