pragma Ada_2022;
package body Agent_Blocks with SPARK_Mode is

   Byte_Base : constant := 256;

   function Encode
     (Item_Role : P.Role; Kind : P.Block_Kind; A : String; B : String := ""; C : String := "";
      Failed : Boolean := False) return String
   is
      Result : String (1 .. Encoded_Length (A, B, C)) := [others => Character'Val (0)];
      procedure Put_Word (Offset : Natural; Value : Natural)
      with Pre => Offset <= P.Block_Header_Bytes - 4 and then Result'Length >= P.Block_Header_Bytes
                  and then Result'First = 1
      is
         V : Natural := Value;
      begin
         for K in 1 .. 4 loop
            Result (Offset + K) := Character'Val (V mod Byte_Base);
            V := V / Byte_Base;
         end loop;
      end Put_Word;
      At_Index : Positive := P.Block_Header_Bytes + 1;
   begin
      Result (P.Block_Role_Offset + 1) := Character'Val (P.Role'Enum_Rep (Item_Role));
      Result (P.Block_Kind_Offset + 1) := Character'Val (P.Block_Kind'Enum_Rep (Kind));
      Result (P.Block_Error_Offset + 1) := Character'Val (if Failed then 1 else 0);
      Put_Word (P.Block_A_Length_Offset, A'Length);
      Put_Word (P.Block_B_Length_Offset, B'Length);
      Put_Word (P.Block_C_Length_Offset, C'Length);
      Result (At_Index .. At_Index + A'Length - 1) := A;
      At_Index := At_Index + A'Length;
      Result (At_Index .. At_Index + B'Length - 1) := B;
      At_Index := At_Index + B'Length;
      Result (At_Index .. At_Index + C'Length - 1) := C;
      return Result;
   end Encode;

   procedure Read (Section : String; At_Index : Positive; View : out Block_View; OK : out Boolean) is
      --  Within the header: At_Index .. At_Index + Block_Header_Bytes - 1.
      function Header_Fits return Boolean is
        (At_Index in Section'Range and then Section'Last < Positive'Last - P.Block_Header_Bytes and then
         Section'Last - At_Index >= P.Block_Header_Bytes - 1);
      function Byte_At (Offset : Natural) return Natural is
        (Character'Pos (Section (At_Index + Offset)))
      with Pre => Offset < P.Block_Header_Bytes and then Header_Fits,
           Post => Byte_At'Result < Byte_Base;
      function Word (Offset : Natural) return Natural is
        (if Byte_At (Offset + 3) /= 0 then Maximum_Section + 1
         else Byte_At (Offset) + Byte_At (Offset + 1) * Byte_Base + Byte_At (Offset + 2) * Byte_Base * Byte_Base)
      with Pre => Offset <= P.Block_Header_Bytes - 4 and then Header_Fits,
           Post => Word'Result <= Maximum_Section + 1;
      Role_Code, Kind_Code, Error_Code : Natural;
      Room : Natural;
   begin
      View := (others => <>);
      OK := False;
      if Section'Last - At_Index + 1 < P.Block_Header_Bytes then
         return;
      end if;
      Role_Code := Byte_At (P.Block_Role_Offset);
      Kind_Code := Byte_At (P.Block_Kind_Offset);
      Error_Code := Byte_At (P.Block_Error_Offset);
      if Role_Code not in P.Role'Enum_Rep (P.User) | P.Role'Enum_Rep (P.Assistant)
        or else Kind_Code not in P.Block_Kind'Enum_Rep (P.Text_Block) .. P.Block_Kind'Enum_Rep (P.Tool_Result_Block)
        or else Error_Code > 1
      then
         return;
      end if;
      View.A_Length := Word (P.Block_A_Length_Offset);
      View.B_Length := Word (P.Block_B_Length_Offset);
      View.C_Length := Word (P.Block_C_Length_Offset);
      Room := Section'Last - At_Index + 1 - P.Block_Header_Bytes;
      if View.A_Length > Room or else View.B_Length > Room - View.A_Length
        or else View.C_Length > Room - View.A_Length - View.B_Length
      then
         return;
      end if;
      View.Item_Role := (if Role_Code = P.Role'Enum_Rep (P.User) then P.User else P.Assistant);
      for K in P.Block_Kind loop
         if P.Block_Kind'Enum_Rep (K) = Kind_Code then
            View.Kind := K;
         end if;
      end loop;
      View.Failed := Error_Code = 1;
      View.A_First := At_Index + P.Block_Header_Bytes;
      View.B_First := View.A_First + View.A_Length;
      View.C_First := View.B_First + View.B_Length;
      View.Next := View.C_First + View.C_Length;
      OK := True;
   end Read;
end Agent_Blocks;
