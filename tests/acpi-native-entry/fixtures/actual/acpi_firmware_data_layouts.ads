pragma Ada_2022;
with Interfaces;
with ACPI_FADT.Transactions;
with ACPI_Firmware_Data_Protocol;
with ACPI_Backend_Binding_Types;
-- Logical geometry only. No capability, mapping, address-space authority or I/O.
package ACPI_Firmware_Data_Layouts with SPARK_Mode, Pure is
   package P renames ACPI_Firmware_Data_Protocol;
   package T renames ACPI_FADT.Transactions;
   subtype Word is Interfaces.Unsigned_64;
   use type Word;
   Page_Bytes : constant Word := P.Wire.Page_Bytes;
   Physical_Limit : constant Word := Page_Bytes * P.Wire.Page_Limit;
   Virtual_Limit : constant Word := ACPI_Backend_Binding_Types.User_Virtual_Limit;
   type Region is record
      Address, Length : Word := 0;
   end record;
   function Valid (Item : Region) return Boolean;
   function Envelope (Item : Region) return P.Envelope with Pre => Valid (Item),
     Post => P.Valid (Envelope'Result)
       and then Envelope'Result.First_Page = Item.Address / Page_Bytes
       and then Envelope'Result.Pages =
         (Item.Address + (Item.Length - 1)) / Page_Bytes - Item.Address / Page_Bytes + 1;
   function Virtual_Valid (Base : Word; Extent : P.Envelope) return Boolean;
   function Mask (Width : T.Access_Width) return Word;
   type Access_Result is record
      Valid : Boolean := False;
      Virtual : Word := 0;
   end record;
   function Resolve (Item : Region; Virtual_Base, Offset : Word;
      Width : T.Access_Width; For_Write : Boolean; Input : Word) return Access_Result
     with Post =>
       (if Resolve'Result.Valid then Valid (Item)
         and then Virtual_Valid (Virtual_Base, Envelope (Item))
         and then Offset < Item.Length
         and then Word (T.Octets (Width)) <= Item.Length - Offset
         and then T.Aligned (Item.Address + Offset, Width)
         and then Resolve'Result.Virtual = Virtual_Base + Item.Address mod Page_Bytes + Offset
         and then Resolve'Result.Virtual < Virtual_Limit
         and then (if For_Write then Input <= Mask (Width) else Input = 0)
        else Resolve'Result.Virtual = 0);
end ACPI_Firmware_Data_Layouts;
