pragma Ada_2022;
package body I2C_Transfer_Wire with SPARK_Mode is
   package G renames CuBit.Grant_References;
   use type Operation;
   function Encode (Item : Request) return Request_Words is
     ([Input_Index => (if Item.Write_Count = 0 then 0 else G.Encode (Item.Input)),
       Output_Index => (if Item.Read_Count = 0 then 0 else G.Encode (Item.Output)),
       Counts_Index => Word (Item.Write_Count) + Word (Item.Read_Count) * Count_Base,
       Deadline_Index => Encode_Deadline (Item.Limit)]);
   function Decode
     (Label : Operation; Protocol : Word; Words : Message_Words;
      Available : Capacity) return Decoding
   is
      Input, Output, Counts : Word;
      Writes, Reads : Count;
      Item : Request;
   begin
      if Label /= Transfer_Operation then return (Status => Wrong_Operation); end if;
      if Protocol /= Protocol_Version then return (Status => Wrong_Protocol); end if;
      if Word (Words'Length) /= Word (Request_Word_Count) then return (Status => Wrong_Length); end if;
      Input := Words (Words'First);
      Output := Words (Words'First + Output_Index);
      Counts := Words (Words'First + Counts_Index);
      if Counts >= Reserved_Base then return (Status => Reserved_Count_Bits); end if;
      Writes := Count (Counts mod Count_Base);
      Reads := Count (Counts / Count_Base);
      if Word (Writes) + Word (Reads) not in 1 .. Word (Available) then
         return (Status => Invalid_Size);
      end if;
      if (Writes = 0 and then Input /= 0) or else
         (Writes /= 0 and then not G.Valid_Wire (Input))
      then return (Status => Invalid_Input); end if;
      if (Reads = 0 and then Output /= 0) or else
         (Reads /= 0 and then not G.Valid_Wire (Output))
      then return (Status => Invalid_Output); end if;
      Item := (Input => <>, Output => <>, Write_Count => Writes,
               Read_Count => Reads,
               Limit => Decode_Deadline (Words (Words'Last)));
      if Writes /= 0 then Item.Input := G.Decode (Input); end if;
      if Reads /= 0 then Item.Output := G.Decode (Output); end if;
      return (Status => Decoded, Value => Item);
   end Decode;
   function Encode_Completion (Item : Completion) return Completion_Words is
     ([Completion_Status_Index => Word (Completion_Status'Enum_Rep (Item.Status))
          + (if Item.Hardware_Succeeded then Hardware_Succeeded_Bit else 0)
          + (if Item.Effects_Possible then Effects_Possible_Bit else 0),
       Ticket_Index => Item.Ticket,
       Completion_Counts_Index => Word (Item.Requested_Writes)
          + Word (Item.Delivered_Reads) * Count_Base,
       Completion_Reserved_Index => 0]);
   function Decode_Completion
     (Label : Operation; Protocol : Word; Words : Message_Words)
      return Completion_Decoding
   is
      Header, Counts, Code : Word;
   begin
      if Label /= Transfer_Reply_Operation then
         return (Status => Completion_Wrong_Operation);
      elsif Protocol /= Protocol_Version then
         return (Status => Completion_Wrong_Protocol);
      elsif Word (Words'Length) /= Word (Completion_Word_Count) then
         return (Status => Completion_Wrong_Length);
      end if;
      Header := Words (Words'First);
      Counts := Words (Words'First + Completion_Counts_Index);
      if Header >= Completion_Reserved_Base or else Counts >= Reserved_Base or else
         Words (Words'Last) /= 0
      then return (Status => Completion_Reserved_Bits); end if;
      Code := Header mod Status_Radix;
      if Code > Word (Completion_Status'Enum_Rep (Completion_Status'Last)) then
         return (Status => Unknown_Status);
      end if;
      return (Status => Completion_Decoded,
         Value => (Status => Completion_Status'Enum_Val (Code),
                   Ticket => Words (Words'First + Ticket_Index),
                   Hardware_Succeeded => (Header / Hardware_Succeeded_Bit) mod 2 = 1,
                   Effects_Possible => (Header / Effects_Possible_Bit) mod 2 = 1,
                   Requested_Writes => Count (Counts mod Count_Base),
                   Delivered_Reads => Count (Counts / Count_Base)));
   end Decode_Completion;
end I2C_Transfer_Wire;
