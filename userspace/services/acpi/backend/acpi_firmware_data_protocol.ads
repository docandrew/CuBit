pragma Ada_2022;
with Interfaces;
with Capability_ABI;
with ACPI_Region_Protocol;
with CuBit.Firmware_Data_Exchange;
-- Backend-to-devmgr ABI only. This unit does not establish AML provenance,
-- acquire capabilities, classify memory or map pages.
package ACPI_Firmware_Data_Protocol with SPARK_Mode, Pure is
   package Wire renames CuBit.Firmware_Data_Exchange;
   package IPC renames ACPI_Region_Protocol;
   subtype Word is Interfaces.Unsigned_64;
   use type Word, IPC.Packet;
   use type Interfaces.Unsigned_8, Interfaces.Unsigned_16, Interfaces.Unsigned_32;
   type Request_ID is new Word;
   No_Request : constant Request_ID := 0;
   type Envelope is record
      First_Page, Pages : Word := 0;
   end record;
   Empty_Envelope : constant Envelope := (others => 0);
   function Valid (Item : Envelope) return Boolean is
     (Wire.Well_Formed (Item.First_Page, Item.Pages, 1));
   -- Exact request encoding model; no request dispatch or grant acquisition.
   function Request_Model (ID : Request_ID; Item : Envelope) return IPC.Packet is
     ((Label => Wire.Data_Window_Request, Length => Wire.Request_Words,
       Flags => 0, Reserved => 0,
       Data => [Wire.First_Page_Word => Item.First_Page,
         Wire.Page_Count_Word => Item.Pages, Wire.Request_Word => Word (ID), others => 0]))
     with Ghost, Global => null;
   function Encode (ID : Request_ID; Item : Envelope) return IPC.Packet
     with Pre => ID /= No_Request and then Valid (Item),
       Post => Encode'Result = Request_Model (ID, Item);
   type Reply (Valid : Boolean := False) is record
      case Valid is
         when True => Disposition : Wire.Disposition;
         when False => null;
      end case;
   end record;
   -- Has_Capability comes independently from receive metadata. Matching
   -- packet flags do not authenticate a sender or the capability's rights.
   function Reply_Accepted (Item : IPC.Packet; ID : Request_ID; Expected : Envelope;
      Has_Capability : Boolean) return Boolean is
     (ID /= No_Request and then Valid (Expected)
      and then Item.Label = Wire.Data_Window_Request and then Item.Length = Wire.Reply_Words
      and then Item.Flags = Capability_ABI.Flag_Byte
        (0, (Capability_Transferred => Has_Capability, others => False))
      and then Item.Reserved = 0
      and then Item.Data (Wire.Reply_Request_Word) = Word (ID)
      and then Wire.Valid_Disposition (Item.Data (Wire.Disposition_Word))
      and then (if Wire.Granted (Wire.Disposition'Enum_Val (Item.Data (Wire.Disposition_Word))) then
         Has_Capability and then Item.Data (Wire.Reply_First_Page_Word) = Expected.First_Page
           and then Item.Data (Wire.Reply_Page_Count_Word) = Expected.Pages
        else not Has_Capability and then Item.Data (Wire.Reply_First_Page_Word) = 0
           and then Item.Data (Wire.Reply_Page_Count_Word) = 0))
     with Ghost, Global => null;
   function Reply_Model (Item : IPC.Packet; ID : Request_ID; Expected : Envelope;
      Has_Capability : Boolean) return Reply is
     (if Reply_Accepted (Item, ID, Expected, Has_Capability) then
        (Valid => True, Disposition => Wire.Disposition'Enum_Val (Item.Data (Wire.Disposition_Word)))
      else (Valid => False)) with Ghost, Global => null;
   function Decode (Item : IPC.Packet; ID : Request_ID; Expected : Envelope;
      Has_Capability : Boolean) return Reply
     with Post => Decode'Result = Reply_Model (Item, ID, Expected, Has_Capability);
end ACPI_Firmware_Data_Protocol;
