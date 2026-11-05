pragma Ada_2022;
package body ACPI_Lock_Trace_Protocol with SPARK_Mode is
   use type ACPI_Region_Protocol.Words;
   use type Interfaces.Unsigned_8, Interfaces.Unsigned_16, Interfaces.Unsigned_32, Interfaces.Unsigned_64;
   function Request return Packet is ((Request_Label,4,0,0,[Version,0,0,0]));
   function Valid_Request (Item : Packet) return Boolean is
     (Item.Label = Request_Label and Item.Length = 4 and Item.Flags = 0 and Item.Reserved = 0
      and Item.Data = ACPI_Region_Protocol.Words'(Version,0,0,0));
   function Reply (Status : Outcome; Records : Natural) return Packet is
     ((Reply_Label,4,0,0,[Version,Outcome'Pos (Status),Interfaces.Unsigned_64 (Natural'Min (Records,12)),0]));
   function Decode (Item : Packet) return Reply_Result is
   begin
      if Item.Label /= Reply_Label or else Item.Length /= 4 or else Item.Flags /= 0 or else Item.Reserved /= 0
        or else Item.Data (0) /= Version or else Item.Data (1) > Outcome'Pos (Outcome'Last)
        or else Item.Data (2) > 12 or else Item.Data (3) /= 0 then return (others => <>); end if;
      return (True,Outcome'Val (Item.Data (1)),Natural (Item.Data (2)));
   end Decode;
end ACPI_Lock_Trace_Protocol;
