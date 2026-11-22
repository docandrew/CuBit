pragma Ada_2022;
with Interfaces;
package body CuBit.Console_Lines with SPARK_Mode is
   use type Interfaces.Unsigned_64;

   procedure Feed (Item : in out Framer; C : Character; Complete : out Boolean) is
   begin
      Complete := False;
      if C = ASCII.LF then
         Complete := True;
      elsif C = ASCII.CR then
         null;
      else
         Item.Used := Item.Used + 1;
         Item.Data (Item.Used) :=
           (if (C < ' ' and then C /= ASCII.HT) or else C = ASCII.DEL then '?' else C);
         Complete := Item.Used = Maximum_Line_Bytes;
      end if;
   end Feed;

   procedure Clear (Item : in out Framer) is
   begin
      Item.Used := 0;
   end Clear;

   procedure Line (Item : Framer; Into : out Line_Buffer; Length : out Line_Length) is
   begin
      Into := Item.Data;
      Length := Item.Used;
   end Line;

   --  A character as a record shows it when its line is not UTF-8.
   function Shown (C : Character) return Character is
     (if Character'Pos (C) > 127 or else (C < ' ' and then C /= ASCII.HT) or else C = ASCII.DEL
      then '?' else C);

   procedure Plain (Text : in out String) is
   begin
      if not Logs.Valid_Text (Text) then
         for I in Text'Range loop
            Text (I) := Shown (Text (I));
            pragma Loop_Invariant
              (for all J in Text'First .. I => Logs.Printable (Text (J)));
         end loop;
      end if;
   end Plain;

   function Is_Console (Value : Logs.Log_Record) return Boolean is
   begin
      for Index in 1 .. Logs.Field_Total (Value) loop
         declare
            Item : constant Logs.Field := Logs.Field_At (Value, Index);
         begin
            if Logs.Has_Name (Item, Console_Field)
              and then Logs."=" (Logs.Kind (Item), Logs.Truth)
              and then Logs.Value (Item) = 1
            then
               return True;
            end if;
         end;
      end loop;
      return False;
   end Is_Console;

end CuBit.Console_Lines;
