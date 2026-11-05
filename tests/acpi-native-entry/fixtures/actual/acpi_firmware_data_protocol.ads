pragma Ada_2022;
with Interfaces;
with ACPI_Region_Protocol;
with CuBit.Firmware_Data_Exchange;
-- Backend-to-devmgr ABI only. This unit does not establish AML provenance,
-- acquire capabilities, classify memory or map pages.
package ACPI_Firmware_Data_Protocol with SPARK_Mode, Pure is
   package Wire renames CuBit.Firmware_Data_Exchange;
   package IPC renames ACPI_Region_Protocol;
   subtype Word is Interfaces.Unsigned_64;
   type Request_ID is new Word;
   No_Request : constant Request_ID := 0;
   type Envelope is record
      First_Page, Pages : Word := 0;
   end record;
   Empty_Envelope : constant Envelope := (others => 0);
   function Valid (Item : Envelope) return Boolean is
     (Wire.Well_Formed (Item.First_Page, Item.Pages, 1));
   function Encode (ID : Request_ID; Item : Envelope) return IPC.Packet
     with Pre => ID /= No_Request and then Valid (Item);
   type Reply (Valid : Boolean := False) is record
      case Valid is
         when True => Disposition : Wire.Disposition;
         when False => null;
      end case;
   end record;
   function Decode (Item : IPC.Packet; ID : Request_ID; Expected : Envelope;
      Has_Capability : Boolean) return Reply;
end ACPI_Firmware_Data_Protocol;
