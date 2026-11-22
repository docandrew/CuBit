------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body Desktop_Address_Editor with SPARK_Mode is

   function Prefix_Image (L : DNA.Prefix_Length) return String
   with Post => Prefix_Image'Result'Length in 1 .. 2;

   function Prefix_Image (L : DNA.Prefix_Length) return String is
      Digit : constant String (1 .. 10) := "0123456789";
   begin
      return (if L >= 10 then [Digit (L / 10 + 1), Digit (L mod 10 + 1)] else [1 => Digit (L + 1)]);
   end Prefix_Image;

   procedure To_Field (F : out Field_Text; Value : String)
   with Pre => Value'Length <= DNA.Maximum_Field, Post => F.Length = Value'Length;

   procedure To_Field (F : out Field_Text; Value : String) is
   begin
      F := (others => <>);
      F.Data (1 .. Value'Length) := Value;
      F.Length := Value'Length;
   end To_Field;

   function DNS_Image (DNS_1, DNS_2 : Unsigned_32) return String
   with Post => DNS_Image'Result'Length <= 32;

   function DNS_Image (DNS_1, DNS_2 : Unsigned_32) return String is
     (if DNS_1 = DNA.No_Address then ""
      elsif DNS_2 = DNA.No_Address then DNA.Image (DNS_1)
      else DNA.Image (DNS_1) & ", " & DNA.Image (DNS_2));

   procedure Open_For (E : out Editor; S : NMR.Interface_Status; DNS_1, DNS_2 : Unsigned_32) is
      F : Field_Set;
   begin
      if S.Settings.Address /= DNA.No_Address then
         To_Field (F (Address_Field), DNA.Image (S.Settings.Address) & "/" & Prefix_Image (S.Settings.Prefix));
      end if;
      if S.Settings.Gateway /= DNA.No_Address then
         To_Field (F (Gateway_Field), DNA.Image (S.Settings.Gateway));
      end if;
      To_Field (F (DNS_Field), DNS_Image (DNS_1, DNS_2));
      E := (Open => True, Name => S.Name, Chosen => Method_Of (S.Setting), Fields => F,
            Focus => Address_Field, In_Force_Method => Method_Of (S.Setting), In_Force => F,
            Previous_Method => Method_Of (S.Setting), Previous => F, Can_Revert => False,
            Result => Editing, Problem => DNA.None);
   end Open_For;

   procedure Close (E : in out Editor) is
   begin
      E.Open := False;
   end Close;

   procedure Set_Field (E : in out Editor; F : Field_Name; Text : String) is
   begin
      if not Editable (E, F) or else (for some Ch of Text => not Accepts (Ch)) then
         return;
      end if;
      To_Field (E.Fields (F), Text);
      E.Result := Editing;
      E.Problem := DNA.None;
   end Set_Field;

   procedure Choose (E : in out Editor; M : Method) is
   begin
      E.Chosen := M;
      if E.Result not in Applying | Reverting then
         E.Result := Editing;
         E.Problem := DNA.None;
      end if;
   end Choose;

   procedure To_Source (Text : String; Source : out Source_Text)
   with Pre => Text'Length <= Maximum_Source, Post => Source.Length = Text'Length;

   procedure To_Source (Text : String; Source : out Source_Text) is
   begin
      Source := (others => <>);
      Source.Data (1 .. Text'Length) := Text;
      Source.Length := Text'Length;
   end To_Source;

   procedure Plan (E : Editor; Source : out Source_Text; DNS_1, DNS_2 : out Unsigned_32;
                   DNS_Changed : out Boolean; Problem : out DNA.Problem) is
      DNS_OK : Boolean;
   begin
      Source := (others => <>);
      Problem := DNA.None;
      DNA.Parse_DNS (Text (E.Fields (DNS_Field)), DNS_1, DNS_2, DNS_OK);
      DNS_Changed := Text (E.Fields (DNS_Field)) /= Text (E.In_Force (DNS_Field));
      case E.Chosen is
         when Automatic => To_Source (DNA.Automatic_Source, Source);
         when Disabled  => To_Source (DNA.Disabled_Source, Source);
         when Static =>
            declare
               Choice : DNA.Static_Choice;
               OK : Boolean;
            begin
               DNA.Parse_Address_Prefix (Text (E.Fields (Address_Field)), Choice.Address, Choice.Length, OK);
               if not OK then
                  Problem := DNA.Address_Malformed;
                  return;
               end if;
               if E.Fields (Gateway_Field).Length > 0 then
                  DNA.Parse_Address (Text (E.Fields (Gateway_Field)), Choice.Gateway, OK);
                  if not OK then
                     Problem := DNA.Gateway_Malformed;
                     return;
                  end if;
               end if;
               Problem := DNA.Check (Choice);
               if Problem /= DNA.None then
                  return;
               end if;
               To_Source (DNA.Static_Source (Choice), Source);
            end;
      end case;
      if not DNS_OK then
         Problem := DNA.DNS_Malformed;
         Source := (others => <>);
      end if;
   end Plan;

   function Shown_CCL (E : Editor) return String is
      Source : Source_Text;
      DNS_1, DNS_2 : Unsigned_32;
      DNS_Changed : Boolean;
      Problem : DNA.Problem;
   begin
      Plan (E, Source, DNS_1, DNS_2, DNS_Changed, Problem);
      if Problem /= DNA.None then
         return "";
      end if;
      return DNA.Setting_Line (E.Name.Text (1 .. E.Name.Length), Image (Source));
   end Shown_CCL;

   procedure Start_Apply (E : in out Editor; Submit : out Boolean) is
      Source : Source_Text;
      DNS_1, DNS_2 : Unsigned_32;
      DNS_Changed : Boolean;
      Problem : DNA.Problem;
   begin
      Submit := False;
      if E.Result in Applying | Reverting then
         return;
      end if;
      Plan (E, Source, DNS_1, DNS_2, DNS_Changed, Problem);
      E.Problem := Problem;
      if E.Problem = DNA.None then
         E.Result := Applying;
         Submit := True;
      end if;
   end Start_Apply;

   procedure Start_Revert (E : in out Editor; Submit : out Boolean) is
   begin
      Submit := False;
      if not E.Can_Revert or else E.Result in Applying | Reverting then
         return;
      end if;
      E.Fields := E.Previous;
      E.Chosen := E.Previous_Method;
      E.Problem := DNA.None;
      E.Result := Reverting;
      Submit := True;
   end Start_Revert;

   procedure Finished (E : in out Editor; OK : Boolean) is
   begin
      case E.Result is
         when Applying =>
            if OK then
               E.Previous := E.In_Force;
               E.Previous_Method := E.In_Force_Method;
               E.In_Force := E.Fields;
               E.In_Force_Method := E.Chosen;
               E.Can_Revert := True;
               E.Result := Applied;
            else
               E.Result := Refused;
            end if;
         when Reverting =>
            if OK then
               E.In_Force := E.Fields;
               E.In_Force_Method := E.Chosen;
               E.Can_Revert := False;
               E.Result := Reverted;
            else
               E.Result := Refused;
            end if;
         when Editing | Applied | Refused | Reverted =>
            null;
      end case;
   end Finished;
end Desktop_Address_Editor;
