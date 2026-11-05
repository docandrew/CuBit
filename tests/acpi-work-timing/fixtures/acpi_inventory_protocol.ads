pragma Ada_2022;
with Interfaces;
with ACPI_Region_Protocol;
with ACPI_FADT.Transactions;
-- Copied metadata only. This protocol cannot install records or scopes and
-- carries no kernel handles. Scalar I/O retains its independent live checks.
package ACPI_Inventory_Protocol with SPARK_Mode, Pure is
   use Interfaces;
   package Wire renames ACPI_Region_Protocol;
   package T renames ACPI_FADT.Transactions;
   use type T.Access_Width;
   Header_Operation : constant Unsigned_32 := 16#100#;
   Part_Operation : constant Unsigned_32 := 16#101#;
   Header_Reply : constant Unsigned_32 := 16#F102#;
   Part_Reply : constant Unsigned_32 := 16#F103#;
   Version : constant Unsigned_64 := 2;
   type Address_Space is (Memory_Space, IO_Space, Embedded_Control_Space);
   EC_Last_Cell : constant Unsigned_64 := 255;
   type Outcome is (Denied, Malformed, Stale, Ready);
   type Part_Kind is (Identity_Part, Region_Part, Register_Part);
   type Payload is array (Natural range 0 .. 2) of Unsigned_64;
   type Parts is array (Part_Kind) of Payload;
   type Width_Set is array (T.Access_Width) of Boolean;
   type Description is record
      ID, Epoch, Base, Length, Offset : Unsigned_64 := 0;
      Space : Address_Space := Memory_Space;
      Readable, Writable, Delegable, Exact_Write_Safe : Boolean := False;
      Widths : Width_Set := [others => False];
      Width : T.Access_Width := T.Byte_Access;
      Write_Mask : Unsigned_64 := 0;
   end record;
   function Width_Mask (Width : T.Access_Width) return Unsigned_64 is
     (case Width is when T.Byte_Access => 16#FF#,
       when T.Word_Access => 16#FFFF#, when T.Dword_Access => 16#FFFF_FFFF#,
       when T.Qword_Access => Unsigned_64'Last);
   function Valid (Item : Description) return Boolean is
     (Item.ID /= 0 and then Item.Epoch /= 0 and then (Item.Space = Embedded_Control_Space or else Item.Base /= 0)
      and then Item.Length /= 0 and then Item.Length - 1 <= Unsigned_64'Last - Item.Base
      and then Item.Offset < Item.Length
      and then Unsigned_64 (T.Octets (Item.Width)) <= Item.Length - Item.Offset
      and then T.Aligned (Item.Base + Item.Offset, Item.Width)
      and then (Item.Readable or Item.Writable) and then Item.Widths (Item.Width)
      and then (Item.Write_Mask and not Width_Mask (Item.Width)) = 0
      and then (if Item.Writable then Item.Exact_Write_Safe else Item.Write_Mask = 0)
      and then (if Item.Space = IO_Space then Item.Base <= Unsigned_64 (Unsigned_16'Last)
        and then Item.Length - 1 <= Unsigned_64 (Unsigned_16'Last) - Item.Base
        and then not Item.Widths (T.Qword_Access))
      and then (if Item.Space = Embedded_Control_Space then
        Item.Base <= EC_Last_Cell
        and then Item.Length - 1 <= EC_Last_Cell - Item.Base
        and then Item.Width = T.Byte_Access
        and then Item.Widths = Width_Set'[T.Byte_Access => True, others => False]));
   function Header_Request return Wire.Packet is
     ((Label => Header_Operation, others => <>));
   function Part_Request
     (Revision, Ordinal : Unsigned_64; Part : Part_Kind) return Wire.Packet is
     ((Label => Part_Operation,
       Data => [Revision, Ordinal, Part_Kind'Pos (Part), 0], others => <>))
     with Pre => Revision /= 0 and then Ordinal /= 0;
   function Envelope_Valid (Item : Wire.Packet) return Boolean is
     (Item.Length = 4 and then Item.Flags = 0 and then Item.Reserved = 0);
   type Header (Valid : Boolean := False) is record
      case Valid is
         when True => Revision, Count : Unsigned_64;
         when False => null;
      end case;
   end record;
   function Decode_Header (Item : Wire.Packet) return Header;
   type Decoded_Part (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Payload;
         when False => null;
      end case;
   end record;
   function Decode_Part (Item : Wire.Packet) return Decoded_Part;
   function Encode (Item : Description) return Parts with Pre => Valid (Item);
   type Decoded_Description (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Description;
         when False => null;
      end case;
   end record;
   function Decode (Item : Parts) return Decoded_Description;
end ACPI_Inventory_Protocol;
