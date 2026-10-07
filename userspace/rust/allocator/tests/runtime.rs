use core::alloc::{GlobalAlloc, Layout};
use cubit_allocator::{BoundedAllocator, MAX_ALIGNMENT};

unsafe extern "C" {
    fn cualloc_test_set_quota(bytes: u64);
    fn cualloc_test_committed() -> u64;
}

// One test owns the process heap (CuAlloc over Linux memory, linked as
// libcubit_allocator.a). Test harness allocations use the host's allocator;
// only the explicit calls below exercise CuAlloc.
#[test]
fn allocation_boundary() {
    let heap = BoundedAllocator;
    unsafe {
        // Every path (small, medium, huge) at every alignment to 1 MiB.
        for size in [1, 15, 16, 17, 255, 4095, 4096, 4097, 65536, 1048576, 3 << 20] {
            for power in 0..=20 {
                let layout = Layout::from_size_align(size, 1 << power).unwrap();
                let p = heap.alloc(layout);
                assert!(!p.is_null(), "size={size}, alignment={}", layout.align());
                assert_eq!(p as usize % layout.align(), 0);
                p.write_bytes(0xa5, size);
                assert!(std::slice::from_raw_parts(p, size).iter().all(|b| *b == 0xa5));
                heap.dealloc(p, layout);
                let z = heap.alloc_zeroed(layout);
                assert!(!z.is_null());
                assert!(std::slice::from_raw_parts(z, size).iter().all(|b| *b == 0));
                heap.dealloc(z, layout);
            }
        }

        // In-class growth, cross-class, small to medium to huge, shrink.
        let small = Layout::from_size_align(31, 16).unwrap();
        let p = heap.alloc(small);
        assert!(!p.is_null());
        for i in 0..31 {
            p.add(i).write(i as u8);
        }
        let q = heap.realloc(p, small, 32);
        assert_eq!(q, p);
        let r = heap.realloc(q, Layout::from_size_align(32, 16).unwrap(), 3000);
        let s = heap.realloc(r, Layout::from_size_align(3000, 16).unwrap(), 20000);
        let t = heap.realloc(s, Layout::from_size_align(20000, 16).unwrap(), 5 << 20);
        assert!(!t.is_null());
        for i in 0..31 {
            assert_eq!(t.add(i).read(), i as u8);
        }
        let u = heap.realloc(t, Layout::from_size_align(5 << 20, 16).unwrap(), 16);
        assert_eq!(u, t);
        heap.dealloc(u, Layout::from_size_align(16, 16).unwrap());

        // No arena bound: far more than one arena of each kind.
        let page = Layout::from_size_align(4096, 4096).unwrap();
        let pages: Vec<*mut u8> = (0..20_000).map(|_| heap.alloc(page)).collect();
        assert!(pages.iter().all(|p| !p.is_null()));
        for (i, p) in pages.iter().enumerate() {
            p.write_bytes((i % 251) as u8, page.size());
        }
        for (i, p) in pages.iter().enumerate() {
            assert!(std::slice::from_raw_parts(*p, page.size()).iter().all(|b| *b == (i % 251) as u8));
        }
        for p in pages {
            heap.dealloc(p, page);
        }
        let big = Layout::from_size_align(200 << 20, 4096).unwrap();
        let b = heap.alloc(big);
        assert!(!b.is_null(), "a 200 MiB block (beyond any one arena)");
        b.write(1);
        b.add(big.size() - 1).write(2);
        heap.dealloc(b, big);

        // Running out (here a quota; on CuBit the process's frame quota):
        // refusals leave live blocks and a failed realloc's block intact.
        let kept = heap.alloc(small);
        kept.write_bytes(0x7b, small.size());
        cualloc_test_set_quota(cualloc_test_committed());
        assert!(heap.alloc(Layout::from_size_align(64 << 20, 16).unwrap()).is_null());
        assert!(heap.realloc(kept, small, 32 << 20).is_null());
        assert!(std::slice::from_raw_parts(kept, small.size()).iter().all(|b| *b == 0x7b));
        cualloc_test_set_quota(u64::MAX);
        assert!(!heap.alloc(Layout::from_size_align(64 << 20, 16).unwrap()).is_null());
        heap.dealloc(kept, small);

        assert!(heap.alloc(Layout::from_size_align(1, MAX_ALIGNMENT * 2).unwrap()).is_null());
    }
}
