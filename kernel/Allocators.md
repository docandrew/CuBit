# BootAllocator

Cubit uses several allocators depending on the scenario. Initially, the
BootAllocator package is used. It has a static bitmap of free physical frames.
This is primarily used by the Memory Manager (Mem_Mgr) to allocate page table
entries used to map all of the physical memory on the system into the
higher-half of memory. Allocations require O(N) time to scan the
bitmap for a free block, frees are constant-time. Please note that the 
BootAllocator deals strictly in _physical_ frames, so conversion of the address
returned may need to be done with Virtmem.P2V.

# BuddyAllocator

The BuddyAllocator uses a modified Knuth buddy allocation scheme, whereby it
sets up a linked list of blocks throughout the free area of memory. It can be
used to allocate large (>= page sized) objects, but does not keep track of the
size of the allocation, so this must be provided to the call to free when you're
finished with the block of memory. It allocates from the head of the linked list
and frees back to the head of the linked list, so allocations and frees are both
very quick. However, if a block must be split to serve the allocation, or
coalesced during free, then it can take extra time. The BuddyAllocator serves
_virtual_ blocks, mapped to LINEAR_BASE and above.

# SlabAllocator

The SlabAllocator can be used as a Storage Pool for a single object size or
type. It uses the BuddyAllocator to get its initial storage, and will find a
block large enough to hold the object size * initial capacity. It uses a scheme
similar to the BuddyAllocator in that it will create a linked list of same-sized
free regions. It allocates and frees to/from the head of the list, so allocations
and frees are both very quick. Since there's no coalescing as with the 
BuddyAllocator, no additional time is ever needed, so this is a O(1) allocator.

If the capacity is exceeded, it will allocate additional blocks up to a limit
set in the Config package.

The SlabAllocator can be used as a Simple_Storage_Pool for an object. See
the LinkedList package for how this is done. The SlabAllocator can additionally
make sure that allocated objects have a specified alignment.

# StoragePool

The StoragePools package is a general-purpose allocator that can be used as the
Simple_Storage_Pool for any object type. It will either use the SlabAllocator
for small objects or the BuddyAllocator for large objects. Different object
sizes will use the same Slab or same-sized BuddyAllocator blocks, so there is a
risk of wasted space here.

CuBit uses `pragma Restrictions (No_Standard_Storage_Pools); ` so to use the
StoragePools allocator, you have to declare a pool object in the package that
you'll be calling `new` and `Unchecked_Deallocation` on. The same storage is
used to service all requests even if different objects are used.

Example:

```
package body MyPackage is

    pool : StoragePools.StoragePool;

    type MyObject is...
    type MyObjectPtr is access MyObject;
    for MyObjectPtr'Simple_Storage_Pool use pool;

    p : MyObjectPtr := new MyObject;

end MyPackage
```

# Notes

Dynamic allocation is generally frowned-upon in Ada, and that is true for CuBit
as well. If possible, use stack objects, but obviously there are times when an
object or array whose size isn't known at runtime needs to outlive the current 
stack frame, and in this situation one of the above allocators should be used.