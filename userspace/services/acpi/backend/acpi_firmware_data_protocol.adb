pragma Ada_2022;
with Capability_ABI;
package body ACPI_Firmware_Data_Protocol with SPARK_Mode is
   use type Interfaces.Unsigned_8, Interfaces.Unsigned_16, Interfaces.Unsigned_32;
   use type Word;
   function Encode (ID : Request_ID; Item : Envelope) return IPC.Packet is
      Result : IPC.Packet := (Label => Wire.Data_Window_Request,
         Length => Wire.Request_Words, others => <>);
   begin
      Result.Data (Wire.First_Page_Word) := Item.First_Page;
      Result.Data (Wire.Page_Count_Word) := Item.Pages;
      Result.Data (Wire.Request_Word) := Word (ID);
      return Result;
   end Encode;
   function Decode (Item : IPC.Packet; ID : Request_ID; Expected : Envelope;
      Has_Capability : Boolean) return Reply is
      Expected_Flags : constant Interfaces.Unsigned_8 := Capability_ABI.Flag_Byte
        (0, (Capability_Transferred => Has_Capability, others => False));
   begin
      if ID = No_Request or else not Valid (Expected)
        or else Item.Label /= Wire.Data_Window_Request or else Item.Length /= Wire.Reply_Words
        or else Item.Flags /= Expected_Flags or else Item.Reserved /= 0
        or else Item.Data (Wire.Reply_Request_Word) /= Word (ID)
        or else not Wire.Valid_Disposition (Item.Data (Wire.Disposition_Word))
      then return (Valid => False); end if;
      declare
         D : constant Wire.Disposition := Wire.Disposition'Enum_Val (Item.Data (Wire.Disposition_Word));
      begin
         if Wire.Granted (D) then
            if not Has_Capability
              or else Item.Data (Wire.Reply_First_Page_Word) /= Expected.First_Page
              or else Item.Data (Wire.Reply_Page_Count_Word) /= Expected.Pages
            then return (Valid => False); end if;
         elsif Has_Capability or else Item.Data (Wire.Reply_First_Page_Word) /= 0
           or else Item.Data (Wire.Reply_Page_Count_Word) /= 0
         then return (Valid => False); end if;
         return (Valid => True, Disposition => D);
      end;
   end Decode;
end ACPI_Firmware_Data_Protocol;
