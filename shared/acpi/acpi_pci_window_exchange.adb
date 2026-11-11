pragma Ada_2022;
package body ACPI_PCI_Window_Exchange with SPARK_Mode is
   function Header (Item : Packet; Label : Interfaces.Unsigned_32; Flags : Word) return Boolean is
     (Item.Label = Label and then Item.Length = Message_Words
      and then Item.Flags = Flags and then Item.Protocol = 0 and then Item.Reserved = 0);
   function Encode (Item : Request) return Packet is
     ((Label => Request_Label, Length => Message_Words, Flags => Accepting_Flags,
       Words => [Word (Item.ID), Word (Item.Key.Segment), Word (Item.Key.Bus),
                 Access_Mode'Enum_Rep (Item.Mode)], others => <>));
   function Decode (Item : Packet; Has_Capability : Boolean) return Decoded_Request is
   begin
      if not Header (Item,Request_Label,Accepting_Flags) or else Has_Capability
        or else Item.Words(0)=0 or else Item.Words(1)>Word(Segment_Number'Last)
        or else Item.Words(2)>Word(Bus_Number'Last)
        or else Item.Words(3)>Access_Mode'Enum_Rep(Read_Write)
      then return (Valid=>False); end if;
      return (Valid=>True,Item=>(ID=>Request_ID(Item.Words(0)),
        Key=>(Segment_Number(Item.Words(1)),Bus_Number(Item.Words(2))),
        Mode=>(if Item.Words(3)=Access_Mode'Enum_Rep(Read_Only) then Read_Only else Read_Write)));
   end Decode;
   function Encode (Item : Reply) return Packet is
     ((Label => Reply_Label, Length => Message_Words,
       Words => [Word(Item.ID), Disposition'Enum_Rep(Item.Status),
                 Word(Item.Record_Number),Item.First], others => <>));
   function Decode (Item : Packet; Expected_ID : Request_ID;
      Has_Capability : Boolean) return Decoded_Reply is
      Result : Reply;
   begin
      if not Header(Item,Reply_Label,(if Has_Capability then Transferred_Flags else 0)) or else Expected_ID=No_Request
        or else Item.Words(0)/=Word(Expected_ID)
      then return (Valid=>False); end if;
      for Status in Disposition loop
         if Item.Words(1)=Disposition'Enum_Rep(Status) then
            Result:=(Expected_ID,Status,Record_ID(Item.Words(2)),Item.Words(3));
            if not Valid(Result) or else Has_Capability /= (Status=Granted) then
               return (Valid=>False);
            end if;
            return (Valid=>True,Item=>Result);
         end if;
      end loop;
      return (Valid=>False);
   end Decode;
end ACPI_PCI_Window_Exchange;
