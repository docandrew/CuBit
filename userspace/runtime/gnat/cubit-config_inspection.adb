package body CuBit.Config_Inspection with SPARK_Mode is
   function Contains (Scope, Key : String) return Boolean is
      Length : Natural := Scope'Length;
   begin
      if Length = 0 then
         return True;
      end if;
      if Scope (Scope'Last) = '.' then
         Length := Length - 1;
      end if;
      if Length = 0 or else Key'Length < Length then
         return False;
      end if;
      if Key (Key'First .. Key'First + (Length - 1)) /=
        Scope (Scope'First .. Scope'First + (Length - 1))
      then
         return False;
      end if;
      return Key'Length = Length or else Key (Key'First + Length) = '.';
   end Contains;
end CuBit.Config_Inspection;
