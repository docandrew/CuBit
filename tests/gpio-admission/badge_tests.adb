pragma Ada_2022;
with Ada.Text_IO;
with GPIO_Channel_Badges;
procedure Badge_Tests is
   package G renames GPIO_Channel_Badges;
   use type G.Word;
   use type G.Decode_Status;
   use type G.Encode_Status;
   use type G.Interrupt_Input;
   use type G.Trigger_Mode;
   use type G.Polarity;
   use type G.Pin_Number;
   Checks : Natural := 0;
   Accepted : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin
      Checks := Checks + 1;
      if not Condition then
         raise Program_Error with "GPIO badge check" & Checks'Image;
      end if;
   end Check;
   procedure Expect (Word : G.Word; Status : G.Decode_Status) is
   begin
      Check (G.Decode (Word).Status = Status);
      Check (G.Valid_Word (Word) = (Status = G.Decoded));
   end Expect;
   --  Literal vectors from the wire contract, independent of codec constants.
   Wire_Prefix : constant G.Word := 16#4750_1000_0000_0000#;
begin
   Check (G.Decode (16#4750_1000_0000_0000#).Value =
      (Pin => 0, Trigger => G.Level_Triggered, Active => G.Active_High));
   Check (G.Decode (16#4750_1000_0001_0000#).Value =
      (Pin => 0, Trigger => G.Level_Triggered, Active => G.Active_Low));
   Check (G.Decode (16#4750_1000_0006_FFFE#).Value =
      (Pin => 65_534, Trigger => G.Edge_Triggered, Active => G.Active_Both));
   Expect (0, G.Wrong_Domain);
   Expect (G.Word'Last, G.Wrong_Domain);
   Expect (16#4932_1000_0000_0001#, G.Wrong_Domain);
   Expect (16#4750_0000_0000_0000#, G.Wrong_Version);
   Expect (16#4750_2000_0000_0000#, G.Wrong_Version);
   Expect (16#4750_1000_0000_FFFF#, G.Missing_Pin);
   Expect (16#4750_1000_0003_0000#, G.Reserved_Polarity);
   Expect (16#4750_1000_0002_0000#, G.Invalid_Trigger);
   for Bit in 19 .. 43 loop
      Expect (Wire_Prefix + 2 ** Bit, G.Reserved_Bits);
   end loop;
   for Version in G.Word range 0 .. 15 loop
      if Version /= 1 then
         Expect (16#4750_0000_0000_0000# + Version * 16#1000_0000_0000#,
            G.Wrong_Version);
      end if;
   end loop;
   for Edge in 0 .. 1 loop
      for Pol in 0 .. 3 loop
         for Pin in 0 .. 65_535 loop
            declare
               Word : constant G.Word := Wire_Prefix + G.Word (Pin)
                 + G.Word (Pol) * 16#1_0000# + G.Word (Edge) * 16#4_0000#;
               Expected : constant G.Decode_Status :=
                 (if Pin = 65_535 then G.Missing_Pin
                  elsif Pol = 3 then G.Reserved_Polarity
                  elsif Edge = 0 and Pol = 2 then G.Invalid_Trigger
                  else G.Decoded);
               D : constant G.Decoding := G.Decode (Word);
            begin
               Check (D.Status = Expected);
               Check (G.Valid_Word (Word) = (Expected = G.Decoded));
               if D.Status = G.Decoded then
                  Accepted := Accepted + 1;
                  Check (D.Value.Pin = G.Pin_Number (Pin));
                  Check (D.Value.Trigger = (if Edge = 1 then G.Edge_Triggered
                                           else G.Level_Triggered));
                  Check (D.Value.Active = (case Pol is when 0 => G.Active_High,
                     when 1 => G.Active_Low, when others => G.Active_Both));
                  declare E : constant G.Encoding := G.Encode (D.Value); begin
                     Check (E.Status = G.Encoded);
                     Check (E.Badge = Word);
                  end;
               end if;
            end;
         end loop;
      end loop;
   end loop;
   Check (Accepted = 327_675);
   for Pin in G.Pin_Number loop
      declare
         V : constant G.Interrupt_Input :=
           (Pin => Pin, Trigger => G.Level_Triggered, Active => G.Active_Both);
      begin
         Check (G.Encode (V).Status = G.Invalid_Input);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("GPIO badge checks" & Checks'Image);
end Badge_Tests;
