pragma Ada_2022;
with CCL.Objects.Views;
with CCL.Typed_Values;

package body System_Generation_Records.Reading is
   package Views renames CCL.Objects.Views;

   LF : constant Character := ASCII.LF;
   Decimal_Base : constant := 10;
   --  Digits of Maximum_Generations: a longer pointer is not one.
   Maximum_Digits : constant := 3;

   --  The checker's value (large: kept here, not on the stack).
   Object : Views.Snapshot;

   function Trimmed_Last (Text : String) return Natural is
      Last : Natural := Text'Last;
   begin
      while Last >= Text'First and then Text (Last) in LF | ASCII.CR loop
         Last := Last - 1;
      end loop;
      return Last;
   end Trimmed_Last;

   procedure Parse_Current (Text : String; N : out Kept_Generation; Valid : out Boolean) is
      Last : constant Natural := Trimmed_Last (Text);
      Value : Natural := 0;
   begin
      N := Image_Generation;
      Valid := Last >= Text'First and then Last - Text'First + 1 <= Maximum_Digits
        and then (for all C of Text (Text'First .. Last) => C in '0' .. '9')
        and then (Text (Text'First) /= '0' or else Last = Text'First);
      if not Valid then
         return;
      end if;
      for C of Text (Text'First .. Last) loop
         Value := Value * Decimal_Base + (Character'Pos (C) - Character'Pos ('0'));
      end loop;
      Valid := Value <= Maximum_Generations;
      if Valid then
         N := Kept_Generation (Value);
      end if;
   end Parse_Current;

   procedure Parse
     (Text : String; Expected : Generation_Number; G : out Generation_Value; Valid : out Boolean)
   is
      Last : constant Natural := Trimmed_Last (Text);
      Checked : CCL.Typed_Values.Check_Result;
   begin
      G := (others => <>);
      Valid := False;
      CCL.Typed_Values.Capture_Value (Schema, Root_Name, Text (Text'First .. Last), Object, Checked);
      if not Checked.Success then
         return;
      end if;
      declare
         Root : constant Views.Cursor := Views.Root (Object);
         Number : constant Unsigned_64 := Views.Scalar (Object, Views.Field (Object, Root, Number_Field)).First;
         Previous : constant Unsigned_64 :=
           Views.Scalar (Object, Views.Field (Object, Root, Previous_Field)).First;
         Pins : constant Views.Cursor := Views.Field (Object, Root, Pins_Field);
         Total : constant Unsigned_64 := Unsigned_64 (Views.Length (Object, Pins));
         Digest_OK : Boolean;
      begin
         if Number /= Unsigned_64 (Expected) or else Previous >= Number or else Total > Maximum_Pins then
            return;
         end if;
         G.Number := Expected;
         G.Previous := Generation_Number (Previous);
         for I in 1 .. Natural (Total) loop
            declare
               Element : constant Views.Cursor := Views.Element (Object, Pins, I);
               Service : constant String := Views.Text (Object, Views.Field (Object, Element, Pin_Service_Field));
               Program : constant String := Views.Text (Object, Views.Field (Object, Element, Pin_Executable_Field));
            begin
               if not Valid_Service_Name (Service) or else Pin_Of (G, Service) /= 0 then
                  return;
               end if;
               G.Count := G.Count + 1;
               G.Pins (G.Count).Length := Service'Length;
               G.Pins (G.Count).Service (1 .. Service'Length) := Service;
               From_Hex (Program, G.Pins (G.Count).Program, Digest_OK);
               if not Digest_OK then
                  return;
               end if;
            end;
         end loop;
      end;
      Valid := True;
   end Parse;

   function Current_Text (N : Kept_Generation) return String is
      Image : constant String := Generation_Number'Image (N);
   begin
      return Image (Image'First + 1 .. Image'Last) & LF;
   end Current_Text;

end System_Generation_Records.Reading;
