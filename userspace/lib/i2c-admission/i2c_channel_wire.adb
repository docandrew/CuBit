pragma Ada_2022;
package body I2C_Channel_Wire with SPARK_Mode is
   function Decode (Raw : Raw_Request; Available : W.Capacity) return Decoding is
      Writes, Reads : W.Count;
   begin
      if Raw.Reserved_A /= 0 or else Raw.Reserved_B /= 0 then return (Status => Reserved_Header); end if;
      if Raw.Counts >= W.Reserved_Base then return (Status => Reserved_Counts); end if;
      Writes := W.Count (Raw.Counts mod W.Count_Base);
      Reads := W.Count (Raw.Counts / W.Count_Base);
      if Word (Writes) + Word (Reads) not in 1 .. Word (Available) then return (Status => Invalid_Size); end if;
      if Raw.Deadline = Word'Last then return (Status => Infinite_Deadline); end if;
      return (Decoded, (Writes, Reads, W.Millisecond (Raw.Deadline)));
   end Decode;
   function Encode (Item : Request; Available : W.Capacity) return Raw_Request is
   begin
      if not Fits (Item, Available) then
         raise Constraint_Error with "I2C channel payload exceeds capacity";
      end if;
      return (Counts => Word (Item.Write_Count) + Word (Item.Read_Count) * W.Count_Base,
         Deadline => Item.Limit, Reserved_A => 0, Reserved_B => 0);
   end Encode;
end I2C_Channel_Wire;
