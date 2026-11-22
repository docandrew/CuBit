------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body RTW89_Efuse with SPARK_Mode is

   --  Records live in Security_Control_Size .. Run_End - 1.
   Run_End : constant := Physical_Size - Security_Control_Size;

   procedure Decode
     (Physical : Physical_Map;
      Logical  : out Logical_Map;
      Records  : out Record_Count;
      Status   : out Decode_Status)
   is
      Position : Natural range Security_Control_Size .. Physical_Size :=
        Security_Control_Size;
      First, Second : Unsigned_8;
      Block    : Natural range 0 .. 255;
      Enabled  : Unsigned_8;
      Target   : Natural;
   begin
      Logical := [others => Unprogrammed];
      Records := 0;
      Status := Decoded;

      while Position < Run_End loop
         pragma Loop_Invariant
           (Position >= Security_Control_Size
            and then Records <= (Position - Security_Control_Size) / Header_Bytes
            and then Status = Decoded);
         pragma Loop_Variant (Increases => Position);

         First := Physical (Position);
         Second := Physical (Position + 1);
         exit when First = Unprogrammed or else Second = Unprogrammed;

         Block := Natural (Shift_Right (Second and High_Nibble, Nibble_Bits))
           + Natural (Shift_Left (First and Low_Nibble, Nibble_Bits));
         Enabled := Second and Low_Nibble;
         Position := Position + Header_Bytes;
         Records := Records + 1;

         for Word in 0 .. Words_Per_Block - 1 loop
            pragma Loop_Invariant
              (Position >= Security_Control_Size + Header_Bytes
               and then Position >= Position'Loop_Entry
               and then Position <= Physical_Size
               and then Records <= (Position - Security_Control_Size) / Header_Bytes
               and then Status = Decoded);
            --  A set bit marks the word absent.
            if (Enabled and Shift_Left (Unsigned_8'(1), Word)) = 0 then
               Target := Block * Block_Bytes + Word * Word_Bytes;
               --  rtw89: phy_idx + 1 > physical - sec_ctrl - 1, or
               --  log_idx + 1 > logical_size.
               if Position + 1 > Run_End - 1 then
                  Status := Word_Past_Physical;
                  return;
               elsif Target + 1 > Logical_Size - 1 then
                  Status := Word_Past_Logical;
                  return;
               end if;
               Logical (Target) := Physical (Position);
               Logical (Target + 1) := Physical (Position + 1);
               Position := Position + Word_Bytes;
            end if;
         end loop;
      end loop;
   end Decode;

end RTW89_Efuse;
