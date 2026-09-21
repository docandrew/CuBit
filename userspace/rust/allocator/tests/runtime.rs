use core::alloc::{GlobalAlloc, Layout};
use cubit_allocator::{ARENA_BYTES, BoundedAllocator, MAX_ALIGNMENT};

// One test owns the singleton heap. Test harness allocations use the host's
// allocator; only the explicit calls below exercise CuBit's implementation.
#[test]
fn allocation_boundary() {
    let heap = BoundedAllocator;
    unsafe {
        for size in [1, 15, 16, 17, 255, 4095, 4096, 4097, 65536, 1048576] {
            for power in 0..=20 {
                let layout = Layout::from_size_align(size, 1 << power).unwrap();
                let p = heap.alloc(layout);
                assert!(!p.is_null(), "size={size}, alignment={}", layout.align());
                assert_eq!(p as usize % layout.align(), 0);
                p.write_bytes(0xa5, size);
                assert!(
                    std::slice::from_raw_parts(p, size)
                        .iter()
                        .all(|b| *b == 0xa5)
                );
                heap.dealloc(p, layout);
                let z = heap.alloc_zeroed(layout);
                assert!(!z.is_null());
                assert!(std::slice::from_raw_parts(z, size).iter().all(|b| *b == 0));
                heap.dealloc(z, layout);
            }
        }

        let small = Layout::from_size_align(31, 16).unwrap();
        let p = heap.alloc(small);
        assert!(!p.is_null());
        for i in 0..31 {
            p.add(i).write(i as u8);
        }
        // In-class growth, cross-class growth, slab-to-extent growth, shrink.
        let q = heap.realloc(p, small, 32);
        assert_eq!(q, p);
        let r = heap.realloc(q, Layout::from_size_align(32, 16).unwrap(), 3000);
        assert!(!r.is_null());
        let s = heap.realloc(r, Layout::from_size_align(3000, 16).unwrap(), 20000);
        assert!(!s.is_null());
        for i in 0..31 {
            assert_eq!(s.add(i).read(), i as u8);
        }
        let t = heap.realloc(s, Layout::from_size_align(20000, 16).unwrap(), 16);
        assert_eq!(t, s);
        heap.dealloc(t, Layout::from_size_align(16, 16).unwrap());

        // Exhaust large memory completely, keep a slab allocation live, and
        // ensure failed realloc preserves both the allocation and its bytes.
        let full = Layout::from_size_align(ARENA_BYTES, MAX_ALIGNMENT).unwrap();
        let large = heap.alloc(full);
        assert!(!large.is_null());
        large.write(0x12);
        large.add(ARENA_BYTES - 1).write(0x34);
        assert!(
            heap.alloc(Layout::from_size_align(4097, 1).unwrap())
                .is_null()
        );
        let p = heap.alloc(small);
        assert!(!p.is_null());
        p.write_bytes(0x7b, small.size());
        assert!(heap.realloc(p, small, 65536).is_null());
        assert!(
            std::slice::from_raw_parts(p, small.size())
                .iter()
                .all(|b| *b == 0x7b)
        );
        assert_eq!(large.read(), 0x12);
        assert_eq!(large.add(ARENA_BYTES - 1).read(), 0x34);
        heap.dealloc(p, small);
        heap.dealloc(large, full);

        // Fragmentation must fail without damaging neighbors; freed adjacent
        // runs immediately form a usable extent without explicit coalescing.
        let quarter = Layout::from_size_align(ARENA_BYTES / 4, 4096).unwrap();
        let blocks = core::array::from_fn::<_, 4, _>(|_| heap.alloc(quarter));
        assert!(blocks.iter().all(|p| !p.is_null()));
        for (i, p) in blocks.iter().enumerate() {
            p.write_bytes(i as u8, quarter.size());
        }
        heap.dealloc(blocks[0], quarter);
        heap.dealloc(blocks[2], quarter);
        let half = Layout::from_size_align(ARENA_BYTES / 2, 4096).unwrap();
        assert!(heap.alloc(half).is_null());
        assert!(
            std::slice::from_raw_parts(blocks[1], quarter.size())
                .iter()
                .all(|b| *b == 1)
        );
        assert!(
            std::slice::from_raw_parts(blocks[3], quarter.size())
                .iter()
                .all(|b| *b == 3)
        );
        heap.dealloc(blocks[1], quarter);
        let joined = heap.alloc(half);
        assert_eq!(joined, blocks[0]);
        heap.dealloc(joined, half);
        heap.dealloc(blocks[3], quarter);

        // Slab exhaustion is independent of extent capacity. Every returned
        // block must be distinct, and the whole slab region must be reusable.
        let page = Layout::from_size_align(4096, 4096).unwrap();
        let mut pages = Vec::new();
        loop {
            let p = heap.alloc(page);
            if p.is_null() {
                break;
            }
            p.write_bytes((pages.len() % 251) as u8, page.size());
            pages.push(p);
            assert!(pages.len() <= ARENA_BYTES / 4096);
        }
        assert_eq!(pages.len(), ARENA_BYTES / 4096);
        for (i, p) in pages.iter().enumerate() {
            assert!(
                std::slice::from_raw_parts(*p, page.size())
                    .iter()
                    .all(|b| *b == (i % 251) as u8)
            );
        }
        for p in pages {
            heap.dealloc(p, page);
        }
        let p = heap.alloc(page);
        assert!(!p.is_null());
        heap.dealloc(p, page);
        assert!(
            heap.alloc(Layout::from_size_align(ARENA_BYTES + 1, 16).unwrap())
                .is_null()
        );
        assert!(
            heap.alloc(Layout::from_size_align(1, MAX_ALIGNMENT * 2).unwrap())
                .is_null()
        );
    }
}
