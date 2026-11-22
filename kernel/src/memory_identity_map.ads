pragma Ada_2022;
with Interfaces;
with System;
with System.Storage_Elements;

-- Kernel-only 64-bit identity index. Caller serializes all access. At most
-- eight levels per lookup; no identity-sized array or capacity-sized scan.
-- Values are opaque, nonzero addresses, never dereferenced by this unit.
generic
   with function Allocate return System.Address;
   with procedure Release (Page : System.Address);
package Memory_Identity_Map is
   type Map is limited private;
   type Insert_Result is (Inserted, Already_Present, Invalid_Argument,
                          Metadata_Limit, No_Memory, Invalid_Backing);
   function Bytes (Object : Map) return Interfaces.Unsigned_64;
   function Find (Object : Map; Identity : Interfaces.Unsigned_64)
     return System.Address;
   procedure Insert
     (Object : in out Map; Identity : Interfaces.Unsigned_64;
      Value : System.Address; Byte_Limit : Interfaces.Unsigned_64;
      Status : out Insert_Result);
   -- Removes only the exact identity/value pair. Empty dynamic branches are
   -- unlinked before Release. At most 7 pages and 7*256 entries examined.
   procedure Remove
     (Object : in out Map; Identity : Interfaces.Unsigned_64;
      Expected : System.Address; Removed : out Boolean);
private
   type Entries is array (Natural range 0 .. 255) of System.Address;
   type Map is limited record
      Root : aliased Entries := [others => System.Null_Address];
      Used_Bytes : Interfaces.Unsigned_64 := 0;
   end record;
end Memory_Identity_Map;
