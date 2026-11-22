with CCL.Declarations;
with CuBit.UI.Theme_Data;

package body CuBit.UI.Theme_CCL with SPARK_Mode is
   use CCL.Declarations;
   use Theme_Data;
   procedure Load (Source : String; Fallback : Theme; Output : out Result) is
      Reader : Scanner;
      Token : Symbol;
      Values : Palette_Colors := Colors (Fallback);
      Seen : array (Field) of Boolean := [others => False];
      Selected : Field := Desktop;
      Found : Boolean;
      Error : Diagnostic := No_Error;
      Error_Position : Positive := 1;
      function Stopped return Boolean is (Failed (Reader) or Error /= No_Error);
      procedure Fail (Why : Diagnostic) is
      begin
         if Error = No_Error then
            Error := Why;
            Error_Position := Position (Reader);
         end if;
      end Fail;
      procedure Expect (Word : String) is
      begin
         Read_Symbol (Reader, Token);
         if not Matches (Token, Word) then Fail (Invalid_Syntax); end if;
      end Expect;
      subtype Channel_Value is RGB_Color range 0 .. 255;
      procedure Channel (Value : out Channel_Value) is
         Numeral : Symbol;
         --  At most three decimal digits; the accumulator cannot exceed 999.
         Number : Unsigned_32 := 0;
      begin
         Value := 0;
         Read_Symbol (Reader, Numeral);
         if Numeral.Length not in 1 .. 3 then Fail (Invalid_Color); return; end if;
         for C of Numeral.Data (1 .. Numeral.Length) loop
            if C not in '0' .. '9' then Fail (Invalid_Color); return; end if;
            Number := Number * 10 + Character'Pos (C) - Character'Pos ('0');
         end loop;
         if Number > 255 then Fail (Invalid_Color); return; end if;
         Value := Number;
      end Channel;
      Red, Green, Blue : Channel_Value;
   begin
      Output := (Value => Fallback, others => <>);
      if Source'Length > Maximum_Source then Output.Error := Too_Long; return; end if;
      Start (Reader, Source);
      Open_Form (Reader); Expect ("theme");
      Read_Symbol (Reader, Token);
      if not Matches (Token, "v1") then Fail (Unsupported_Version); end if;
      Open_Form (Reader); Expect ("base"); Read_Symbol (Reader, Token);
      if Matches (Token, "alloy-light") then Values := Colors (CuBit_Alloy);
      elsif Matches (Token, "alloy-dark") then Values := Colors (CuBit_Alloy_Dark);
      else Fail (Invalid_Base);
      end if;
      Close_Form (Reader);
      while not Stopped and then not At_Close (Reader) and then not At_End (Reader) loop
         Open_Form (Reader); Read_Symbol (Reader, Token);
         Found := False;
         for Item in Field loop
            if Matches (Token, Name (Item)) then Selected := Item; Found := True; end if;
         end loop;
         if not Found then Fail (Unknown_Field); exit; end if;
         if Seen (Selected) then Fail (Duplicate_Field); exit; end if;
         Seen (Selected) := True;
         Open_Form (Reader); Expect ("rgb");
         Channel (Red); Channel (Green); Channel (Blue);
         Close_Form (Reader); Close_Form (Reader);
         if not Stopped then Values (Selected) := Red * 65536 + Green * 256 + Blue; end if;
      end loop;
      Close_Form (Reader);
      if Failed (Reader) then Fail (Invalid_Syntax);
      elsif not At_End (Reader) then Fail (Trailing_Input);
      end if;
      --  Parsing mutates only local candidate/error state. Publish once, so
      --  neither parser failures nor loop reasoning can alter the fallback.
      if Stopped then
         Output := (False, Error, Error_Position, Fallback);
      else
         Output := (True, No_Error, 1, To_Theme (Values));
      end if;
   end Load;
end CuBit.UI.Theme_CCL;
