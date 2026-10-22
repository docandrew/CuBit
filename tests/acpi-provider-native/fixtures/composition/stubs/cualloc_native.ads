pragma Ada_2022;
with Interfaces; use Interfaces;
package CuAlloc_Native is
 function Allocate_Zeroed(Bytes,Alignment : Unsigned_64) return Unsigned_64;
 function Usable_Size(Item : Unsigned_64) return Unsigned_64;
 -- No Free binding: adding a release call to the bridge must not compile here.
end CuAlloc_Native;
