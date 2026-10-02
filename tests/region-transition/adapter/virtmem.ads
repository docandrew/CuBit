-- Hosted address translation stand-in, not the kernel's direct map.
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
package Virtmem is
   PAGE_SIZE : constant := 4096;
   subtype VirtAddress is Integer_Address;
   subtype PhysAddress is Integer_Address;
   subtype PFN is Unsigned_64 range 0 .. 2 ** 36 - 1;
   subtype PageTableIndex is Natural range 0 .. 511;
   type Bits_3 is mod 8;
   type Bits_11 is mod 2048;
   type Frame_Bits is mod 2 ** 40;
   type PageTableEntry is record
      present, writable, user, writeThrough, cacheDisabled, accessed, dirty,
      size, global : Boolean := False;
      undefinedA : Bits_3 := 0;
      pgNum : Frame_Bits := 0;
      undefinedB : Bits_11 := 0;
      NX : Boolean := False;
   end record with Size => 64;
   for PageTableEntry use record
      present at 0 range 0 .. 0; writable at 0 range 1 .. 1;
      user at 0 range 2 .. 2; writeThrough at 0 range 3 .. 3;
      cacheDisabled at 0 range 4 .. 4; accessed at 0 range 5 .. 5;
      dirty at 0 range 6 .. 6; size at 0 range 7 .. 7;
      global at 0 range 8 .. 8; undefinedA at 0 range 9 .. 11;
      pgNum at 0 range 12 .. 51; undefinedB at 0 range 52 .. 62;
      NX at 0 range 63 .. 63;
   end record;
   type P4 is array (PageTableIndex) of PageTableEntry with Alignment => 4096;
   type P3 is array (PageTableIndex) of PageTableEntry with Alignment => 4096;
   type P2 is array (PageTableIndex) of PageTableEntry with Alignment => 4096;
   type P1 is array (PageTableIndex) of PageTableEntry with Alignment => 4096;
   function getP4Index (V : VirtAddress) return PageTableIndex is
     (Natural (Shift_Right (Unsigned_64 (V), 39) and 511));
   function getP3Index (V : VirtAddress) return PageTableIndex is
     (Natural (Shift_Right (Unsigned_64 (V), 30) and 511));
   function getP2Index (V : VirtAddress) return PageTableIndex is
     (Natural (Shift_Right (Unsigned_64 (V), 21) and 511));
   function getP1Index (V : VirtAddress) return PageTableIndex is
     (Natural (Shift_Right (Unsigned_64 (V), 12) and 511));
   function P2V (V : PhysAddress) return Integer_Address is (V);
   generic
      type PN is array (PageTableIndex) of PageTableEntry;
   function getNextTable (Table : PN; Index : PageTableIndex) return PhysAddress;
end Virtmem;
