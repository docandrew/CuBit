package body Config_Authority_Wire with SPARK_Mode is
   procedure Decode
     (Data : String; Rules : out Config_Authority.Rule_Set; Accepted : out Boolean)
   is
      Candidate : Config_Authority.Rule_Set;
      Added : Boolean;
   begin
      Rules := Config_Authority.Empty_Rules;
      Accepted := False;
      if Data'Length = 0 or else Data'Length > Maximum_Bytes or else
        Data'Length mod Entry_Bytes /= 0
      then
         return;
      end if;
      for I in 0 .. Data'Length / Entry_Bytes - 1 loop
         declare
            First : constant Integer := Data'First + I * Entry_Bytes;
            Item : constant String (1 .. Entry_Bytes) :=
              Data (First .. First + (Entry_Bytes - 1));
            Mask : constant Natural := Character'Pos (Item (1));
            Length : constant Natural := Character'Pos (Item (2));
         begin
            if Length > Config_Authority.Maximum_Scope or else Mask > 3 or else
              (for some J in 3 .. 8 => Item (J) /= Character'Val (0))
            then
               return;
            end if;
            Config_Authority.Append
              (Candidate, Item (9 .. 8 + Length),
               [Config_Authority.Read_Config => Mask mod 2 = 1,
                Config_Authority.Write_Config => Mask >= 2,
                Config_Authority.Activate_Config => False], Added);
            if not Added then return; end if;
         end;
      end loop;
      Rules := Candidate;
      Accepted := True;
   end Decode;
end Config_Authority_Wire;
