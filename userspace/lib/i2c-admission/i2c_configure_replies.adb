pragma Ada_2022;
package body I2C_Configure_Replies with SPARK_Mode is
   function Encode (Value : Reply) return Reply_Words is
     ([Version_Index => Version,
       Status_Index => Status_Code (Value.Status),
       Stage_Index => (if Value.Status = Ready then 0 else Word (Startup_Stage'Pos (Value.Stage))),
       Value_Index => (if Value.Status = Ready then 0 else Word (Value.Last_Register_Value))]);
   function Valid_Words (Words : Message_Words) return Boolean is
     (Word (Words'Length) = Word (Reply_Word_Count)
      and then Words (Words'First) = Version
      and then Words (Words'First + Status_Index) <= Failed_Effects_Possible_Code
      and then (if Words (Words'First + Status_Index) = Ready_Code then
         Words (Words'First + Stage_Index) = 0 and then Words (Words'Last) = 0
       else Words (Words'First + Stage_Index) <= Word (Startup_Stage'Pos (Startup_Stage'Last))
         and then Words (Words'Last) <= Word (Register_Value'Last)));
   function Decode (Words : Message_Words) return Decoding is
   begin
      if Word (Words'Length) /= Word (Reply_Word_Count) then
         return (Status => Wrong_Length);
      elsif Words (Words'First) /= Version then
         return (Status => Wrong_Version);
      end if;
      declare
         -- A four-word frame guarantees these index additions cannot overflow.
         Status : constant Word := Words (Words'First + Status_Index);
         Stage : constant Word := Words (Words'First + Stage_Index);
         Value : constant Word := Words (Words'Last);
      begin
         if Status > Failed_Effects_Possible_Code then
            return (Status => Invalid_Status);
         elsif Status = Ready_Code then
            if Stage /= 0 or else Value /= 0 then return (Status => Noncanonical_Ready); end if;
            return (Status => Decoded, Value => (Status => Ready));
         elsif Stage > Word (Startup_Stage'Pos (Startup_Stage'Last)) then
            return (Status => Invalid_Stage);
         elsif Value > Word (Register_Value'Last) then
            return (Status => Invalid_Register_Value);
         else
            return (Status => Decoded,
                    Value => (Status => Status_Of (Status),
                              Stage => Startup_Stage'Val (Natural (Stage)),
                              Last_Register_Value => Register_Value (Value)));
         end if;
      end;
   end Decode;
   procedure Round_Trip (Value : Reply) is
   begin
      pragma Assert (Decode (Encode (Value)).Status = Decoded);
      pragma Assert (Decode (Encode (Value)).Value = Value);
   end Round_Trip;
end I2C_Configure_Replies;
