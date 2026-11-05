package body CuBit.Config_Protocol with SPARK_Mode is
   use type Interfaces.Unsigned_64;
   procedure Decode
     (Op : Operation; Word_Count : Natural;
      Key, Value : Interfaces.Unsigned_64;
      Bounds : out Request_Bounds; Valid : out Boolean) is
   begin
      Bounds := (others => <>);
      Valid := False;
      if Word_Count /= 4 or Key > Maximum_Key or Value > Maximum_Value or
        (Key = 0 and Op /= List_Keys) or (Value /= 0 and Op /= Set_Value)
      then
         return;
      end if;
      Bounds.Key := Natural (Key);
      Bounds.Value := Natural (Value);
      Bounds.Input_Bytes := Bounds.Key + Bounds.Value;
      Bounds.Mapping_Bytes :=
        (case Op is
           when Get_Value | List_Keys => Maximum_Value,
           when Set_Value | Delete_Value => Bounds.Input_Bytes);
      Valid := True;
   end Decode;
end CuBit.Config_Protocol;
