package body Capacity_Fixture is
   use AML_Decode;
   function Text (S : String) return Bytes is
      R : Bytes (1 .. S'Length);
   begin
      for I in R'Range loop R (I) := Character'Pos (S (S'First + I - 1)); end loop;
      return R;
   end Text;
   function Method (Name : String; Code : Bytes) return Bytes is
      Payload : constant Bytes := Text (Name) & [0] & Code;
      Count : Positive := 1;
      Length : Natural;
      Encoded : Bytes (1 .. 4) := [others => 0];
   begin
      while Payload'Length + Count > (case Count is
        when 1 => 63, when 2 => 4095, when 3 => 1048575, when others => 268435455)
      loop Count := Count + 1; end loop;
      Length := Payload'Length + Count;
      if Count = 1 then Encoded (1) := Byte (Length);
      else
         Encoded (1) := Byte ((Count - 1) * 64 + Length mod 16);
         Length := Length / 16;
         for I in 2 .. Count loop Encoded (I) := Byte (Length mod 256); Length := Length / 256; end loop;
      end if;
      return Bytes'[16#14#] & Encoded (1 .. Count) & Payload;
   end Method;
   function Body_Of_Size (Size : Natural) return Bytes is
      R : Bytes (1 .. Size) := [others => 16#A3#];
   begin
      if Size >= 2 then R (1 .. 2) := [16#A4#,1]; end if;
      return R;
   end Body_Of_Size;
end Capacity_Fixture;
