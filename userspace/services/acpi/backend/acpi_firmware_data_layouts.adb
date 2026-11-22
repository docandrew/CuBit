pragma Ada_2022;
package body ACPI_Firmware_Data_Layouts with SPARK_Mode is
   function Valid (Item : Region) return Boolean is
     (Item.Address < Physical_Limit and then Item.Length > 0
      and then Item.Length <= Physical_Limit - Item.Address);
   function Envelope (Item : Region) return P.Envelope is
     ((Item.Address / Page_Bytes,
       (Item.Address + (Item.Length - 1)) / Page_Bytes - Item.Address / Page_Bytes + 1));
   function Virtual_Valid (Base : Word; Extent : P.Envelope) return Boolean is
     (P.Valid (Extent) and then Base > 0 and then Base < Virtual_Limit
      and then Base mod Page_Bytes = 0
      and then Extent.Pages <= (Virtual_Limit - Base) / Page_Bytes);
   function Mask (Width : T.Access_Width) return Word is
     (case Width is when T.Byte_Access => 16#FF#, when T.Word_Access => 16#FFFF#,
        when T.Dword_Access => 16#FFFF_FFFF#, when T.Qword_Access => Word'Last);
   function Resolve (Item : Region; Virtual_Base, Offset : Word;
      Width : T.Access_Width; For_Write : Boolean; Input : Word) return Access_Result is
      Bytes : constant Word := Word (T.Octets (Width));
   begin
      if not Valid (Item) or else not Virtual_Valid (Virtual_Base, Envelope (Item))
        or else Offset >= Item.Length or else Bytes > Item.Length - Offset
        or else not T.Aligned (Item.Address + Offset, Width)
        or else (if For_Write then Input > Mask (Width) else Input /= 0)
      then return (others => <>); end if;
      return (True, Virtual_Base + Item.Address mod Page_Bytes + Offset);
   end Resolve;
end ACPI_Firmware_Data_Layouts;
