------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Host names that stand behind placeholder addresses (docs/c-removal.md):
--  CuBit programs do not resolve names, netstack does when a connection
--  opens inside the program's scope. getaddrinfo gives a name a placeholder
--  IPv4 address 100.100.x.y, and connecting to it opens
--  "@net:tcp:<name>:<port>".
--
--  @description
--  Names are kept inline (Capacity of them, each up to Name_Bytes), matched
--  without regard to case, as DNS does. Proved (tests/libc-ada): every
--  index stays in range, a name found is the one asked for, and a full
--  table refuses rather than overwrites.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Libc_Net_Names with Pure, SPARK_Mode is

   Capacity   : constant := 1_024;
   Name_Bytes : constant := 255;
   --  Placeholder addresses: Placeholder_Network | index, index 1 .. Capacity.
   Placeholder_Network : constant Unsigned_32 := 16#6464_0000#;     --  100.100.0.0
   Index_Mask : constant Unsigned_32 := 16#FFFF#;

   subtype Name_Index is Positive range 1 .. Capacity;
   subtype Name_Count is Natural range 0 .. Capacity;
   subtype Name_Length is Natural range 1 .. Name_Bytes;

   type Name_Entry is record
      Text   : String (1 .. Name_Bytes) := [others => ' '];
      Length : Natural range 0 .. Name_Bytes := 0;
   end record;
   type Name_Entries is array (Name_Index) of Name_Entry;
   type Table is record
      Entries : Name_Entries;
      Count   : Name_Count := 0;
   end record;

   function Lower (C : Character) return Character is
     (if C in 'A' .. 'Z' then Character'Val (Character'Pos (C) + 32) else C);

   function Same (A, B : String) return Boolean is
     (A'Length = B'Length
      and then (for all K in 0 .. A'Length - 1 =>
                  Lower (A (A'First + K)) = Lower (B (B'First + K))));

   --  The index of Name, added if new; 0 if the table is full.
   procedure Index_Of (T : in out Table; Name : String; Index : out Name_Count)
   with Pre => Name'Length in Name_Length,
        Post => (if Index /= 0 then Index <= T.Count
                   and then Same (T.Entries (Index).Text (1 .. T.Entries (Index).Length), Name));

   --  The placeholder (host order) for an index, and back.
   function Placeholder (Index : Name_Index) return Unsigned_32 is
     (Placeholder_Network or Unsigned_32 (Index));
   function Index_From (Address : Unsigned_32) return Natural is
     (if (Address and not Index_Mask) = Placeholder_Network
        and then (Address and Index_Mask) in 1 .. Capacity
      then Natural (Address and Index_Mask) else 0);

end CuBit.Libc_Net_Names;
