with Interfaces; use Interfaces;
generic
   type Element is private;
   Empty_Element : Element;
   Bootstrap_Count : Positive := 16;
package Intel_GPU_Record_Store is
   type Store is limited private;
   function Capacity (Object : Store) return Positive;
   function Get (Object : Store; Index : Positive) return Element
     with Pre => Index <= Capacity (Object);
   procedure Put (Object : in out Store; Index : Positive; Value : Element)
     with Pre => Index <= Capacity (Object);
   -- Trusted writable CPU metadata mapping, retained for the store lifetime;
   -- never app addresses, MMIO, or GPU BO backing. Caller commits before this
   -- operation. Base is immutable, extension <=64KiB, old records never move.
   procedure Extend
     (Object : in out Store; Base, Bytes : Unsigned_64; Accepted : out Boolean);
private
   type Inline_Items is array (Positive range 1 .. Bootstrap_Count) of Element;
   type Store is limited record
      Inline : Inline_Items := [others => Empty_Element];
      Available : Positive := Bootstrap_Count;
      Base, Bytes : Unsigned_64 := 0;
   end record;
end Intel_GPU_Record_Store;
