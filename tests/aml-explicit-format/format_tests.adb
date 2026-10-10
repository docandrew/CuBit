with Ada.Text_IO;
with AML_Decode;
with AML_Explicit_Formatting;
procedure Format_Tests is
   use AML_Decode;
   package F is new AML_Explicit_Formatting (65_536);
   package Zero is new AML_Explicit_Formatting (0);
   package One is new AML_Explicit_Formatting (1);
   package Three is new AML_Explicit_Formatting (3);
   use type F.Build_Status;
   use type Zero.Build_Status;
   use type One.Build_Status;
   use type Three.Build_Status;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with "format check" & Checks'Image; end if;
   end Check;
   function Octets (Text : String) return Bytes is
      R : Bytes (1 .. Text'Length);
   begin
      for I in Text'Range loop R (I - Text'First + 1) := Character'Pos (Text (I)); end loop;
      return R;
   end Octets;
   procedure Int (Mode : F.Format_Mode; Width : Integer_Width; V : Integer_Value; Text : String) is
      R : constant F.Result := F.From_Integer (Mode, Width, V);
   begin
      Check (R.Status = F.Built);
      Check (R.Length = Text'Length and then R.Data = Octets (Text));
   end Int;
   procedure Buf (Mode : F.Format_Mode; Data : Bytes; Text : String) is
      Before : constant Bytes := Data;
      R : constant F.Result := F.From_Buffer (Mode, Data);
   begin
      Check (R.Status = F.Built);
      Check (R.Length = Text'Length and then R.Data = Octets (Text));
      Check (Data = Before);
   end Buf;
   Mixed : constant Bytes := [0, 9, 10, 15, 16, 99, 100, 255];
   High : constant Bytes (Positive'Last - 7 .. Positive'Last) := Mixed;
   Alphabet : constant String := "0123456789ABCDEF";
begin
   for Width in Integer_Width loop
      Int (F.Decimal_Format, Width, 0, "0"); Int (F.Hexadecimal_Format, Width, 0, "0x0");
      Int (F.Decimal_Format, Width, 9, "9"); Int (F.Decimal_Format, Width, 10, "10");
      Int (F.Decimal_Format, Width, 99, "99"); Int (F.Decimal_Format, Width, 100, "100");
      Int (F.Decimal_Format, Width, 999, "999"); Int (F.Decimal_Format, Width, 1000, "1000");
      Int (F.Hexadecimal_Format, Width, 15, "0xF"); Int (F.Hexadecimal_Format, Width, 16, "0x10");
      Int (F.Hexadecimal_Format, Width, 255, "0xFF"); Int (F.Hexadecimal_Format, Width, 256, "0x100");
      Int (F.Hexadecimal_Format, Width, 16#ABCDEF#, "0xABCDEF");
      Int (F.Decimal_Format, Width, Integer_Value'Last,
        (if Width = Bits_32 then "4294967295" else "18446744073709551615"));
      Int (F.Hexadecimal_Format, Width, Integer_Value'Last,
        (if Width = Bits_32 then "0xFFFFFFFF" else "0xFFFFFFFFFFFFFFFF"));
      Int (F.Decimal_Format, Width, 16#1_0000_0000#,
        (if Width = Bits_32 then "0" else "4294967296"));
      Int (F.Hexadecimal_Format, Width, 16#FEDC_BA98_7654_3210#,
        (if Width = Bits_32 then "0x76543210" else "0xFEDCBA9876543210"));
      for N in 0 .. 255 loop
         declare
            Image : constant String := N'Image;
            Hex : constant String := (if N < 16 then "0x" & Alphabet (N + 1)
              else "0x" & Alphabet (N / 16 + 1) & Alphabet (N mod 16 + 1));
         begin
            Int (F.Decimal_Format, Width, Integer_Value (N), Image (2 .. Image'Last));
            Int (F.Hexadecimal_Format, Width, Integer_Value (N), Hex);
         end;
      end loop;
   end loop;
   for N in 0 .. 255 loop
      declare
         Image : constant String := N'Image;
         Hex : constant String := "0x" & Alphabet (N / 16 + 1) & Alphabet (N mod 16 + 1);
      begin
         Buf (F.Decimal_Format, Bytes'(1 => Byte (N)), Image (2 .. Image'Last));
         Buf (F.Hexadecimal_Format, Bytes'(1 => Byte (N)), Hex);
      end;
   end loop;
   Buf (F.Decimal_Format, Mixed, "0,9,10,15,16,99,100,255");
   Buf (F.Hexadecimal_Format, Mixed, "0x00,0x09,0x0A,0x0F,0x10,0x63,0x64,0xFF");
   Buf (F.Decimal_Format, High, "0,9,10,15,16,99,100,255");
   Buf (F.Hexadecimal_Format, High, "0x00,0x09,0x0A,0x0F,0x10,0x63,0x64,0xFF");
   Buf (F.Decimal_Format, Bytes'(1 .. 0 => 0), ""); Buf (F.Hexadecimal_Format, Bytes'(1 .. 0 => 0), "");
   declare
      R0 : constant Zero.Result := Zero.From_Buffer (Zero.Decimal_Format, Bytes'(1 .. 0 => 0));
      RI : constant Zero.Result := Zero.From_Integer (Zero.Decimal_Format, Bits_64, 0);
      R1 : constant One.Result := One.From_Integer (One.Decimal_Format, Bits_64, 9);
      R2 : constant One.Result := One.From_Integer (One.Decimal_Format, Bits_64, 10);
      R3 : constant Three.Result := Three.From_Integer (Three.Hexadecimal_Format, Bits_64, 15);
      R4 : constant Three.Result := Three.From_Integer (Three.Hexadecimal_Format, Bits_64, 16);
      RB : constant Three.Result := Three.From_Buffer (Three.Decimal_Format, Bytes'(1 => 255));
      RF : constant Three.Result := Three.From_Buffer (Three.Hexadecimal_Format, Bytes'(1 => 0));
   begin
      Check (R0.Status = Zero.Built);
      Check (RI.Status = Zero.Length_Limit);
      Check (R1.Status = One.Built and then R1.Data = Bytes'(1 => 57));
      Check (R2.Status = One.Length_Limit and then R2.Length = 0);
      Check (R3.Status = Three.Built and then R3.Data = Octets ("0xF"));
      Check (R4.Status = Three.Length_Limit and then R4.Length = 0);
      Check (RB.Status = Three.Built and then RB.Data = Octets ("255"));
      Check (RF.Status = Three.Length_Limit and then RF.Length = 0);
   end;
   declare
      Fit : constant Bytes (1 .. 13_107) := [others => 255];
      Over : constant Bytes (1 .. 13_108) := [others => 255];
      R : constant F.Result := F.From_Buffer (F.Hexadecimal_Format, Fit);
      Bad : constant F.Result := F.From_Buffer (F.Hexadecimal_Format, Over);
      DFit : constant Bytes (1 .. 16_384) := [others => 255];
      DOver : constant Bytes (1 .. 16_385) := [others => 255];
      D : constant F.Result := F.From_Buffer (F.Decimal_Format, DFit);
      DBad : constant F.Result := F.From_Buffer (F.Decimal_Format, DOver);
   begin
      Check (R.Status = F.Built and then R.Length = 65_534);
      Check (R.Data (1 .. 4) = Octets ("0xFF") and then R.Data (R.Length - 3 .. R.Length) = Octets ("0xFF"));
      Check (Bad.Status = F.Length_Limit and then Bad.Length = 0);
      Check (D.Status = F.Built and then D.Length = 65_535);
      Check (D.Data (1 .. 3) = Octets ("255") and then D.Data (D.Length - 2 .. D.Length) = Octets ("255"));
      Check (DBad.Status = F.Length_Limit and then DBad.Length = 0);
   end;
   -- Negative recognizer witnesses are assertion checks, not release counter claims.
   pragma Assert (not F.Integer_Encoding (F.Decimal_Format, Bits_64, 1, Octets ("01")));
   pragma Assert (not F.Integer_Encoding (F.Hexadecimal_Format, Bits_64, 15, Octets ("0xf")));
   pragma Assert (not F.Integer_Encoding (F.Decimal_Format, Bits_64, 0, Octets ("18446744073709551616")));
   pragma Assert (not F.Buffer_Encoding (F.Hexadecimal_Format, Bytes'(1 => 0), Octets ("0x0")));
   pragma Assert (not F.Buffer_Encoding (F.Decimal_Format, Bytes'(1 => 0), Octets ("0,")));
   Ada.Text_IO.Put_Line ("Explicit formatting checks" & Checks'Image);
end Format_Tests;
