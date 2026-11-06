with Ada.Unchecked_Deallocation;

package body Remote_Activity is
   use Interfaces;

   type Ring is array (Entry_Number range <>) of Activity;
   type Ring_Access is access Ring;
   procedure Free is new Ada.Unchecked_Deallocation (Ring, Ring_Access);

   Entries : Ring_Access := null;
   --  The oldest entry's slot, and how many slots hold entries.
   First : Entry_Number := 1;
   Used : Entry_Count := 0;
   Total : Unsigned_64 := 0;

   function Shown (Text : String) return String is
      Result : String (1 .. Maximum_Argument_Shown) := [others => ' '];
      Last : Argument_Length := 0;
      Cut : Boolean := False;
      C : Character;
   begin
      for I in Text'Range loop
         C := (if Text (I) in ' ' .. '~' then Text (I) else ' ');
         if C = ' ' and then (Last = 0 or else Result (Last) = ' ') then
            null;   --  no leading or repeated space
         elsif Last = Maximum_Argument_Shown then
            Cut := True;
            exit;
         else
            Last := Last + 1;
            Result (Last) := C;
         end if;
      end loop;
      if Cut then
         Last := Maximum_Argument_Shown - Ellipsis'Length;
         while Last > 0 and then Result (Last) = ' ' loop
            Last := Last - 1;
         end loop;
         Result (Last + 1 .. Last + Ellipsis'Length) := Ellipsis;
         Last := Last + Ellipsis'Length;
      elsif Last > 0 and then Result (Last) = ' ' then
         Last := Last - 1;
      end if;
      return Result (1 .. Last);
   end Shown;

   function Slot (Number : Entry_Number) return Entry_Number is
     (Entry_Number ((Natural (First) - 1 + Natural (Number) - 1) mod Entries'Length + 1));

   --  A ring twice the size (up to the ceiling), holding the entries oldest
   --  first; nothing at the ceiling. (A heap that cannot give at most
   --  Maximum_Entries entries ends the process: the user runtime has no
   --  allocation failure to recover from.)
   procedure Grow is
      Size : constant Entry_Count :=
        (if Entries = null then Initial_Entries
         else Entry_Count'Min (Maximum_Entries, 2 * Entries'Length));
      Larger : Ring_Access;
   begin
      if Entries /= null and then Entries'Length = Maximum_Entries then
         return;
      end if;
      Larger := new Ring (1 .. Size);
      if Entries /= null then
         for N in 1 .. Used loop
            Larger (N) := Entries (Slot (N));
         end loop;
         Free (Entries);
      end if;
      Entries := Larger;
      First := 1;
   end Grow;

   procedure Add
     (Time_Ms : Unsigned_64; Op : Kind; Argument : String; Result : Outcome)
   is
      Text : constant String := Shown (Argument);
      Item : Activity := (Time_Ms => Time_Ms, Op => Op, Result => Result, others => <>);
   begin
      Item.Argument (1 .. Text'Length) := Text;
      Item.Argument_Last := Text'Length;
      if Entries = null or else Used = Entries'Length then
         Grow;
      end if;
      if Used < Entries'Length then
         Used := Used + 1;
      else
         --  At the ceiling: the oldest gives way.
         First := (if First = Entries'Last then 1 else First + 1);
      end if;
      Entries (Slot (Used)) := Item;
      Total := Total + 1;
   end Add;

   function Count return Entry_Count is (Used);
   function Entry_At (Number : Entry_Number) return Activity is (Entries (Slot (Number)));
   function Added return Unsigned_64 is (Total);
   function Capacity return Entry_Count is (if Entries = null then 0 else Entries'Length);

   function Line (Item : Activity) return String is
     (Kind_Name (Item.Op) & (if Item.Argument_Last = 0 then "" else " " & Argument_Of (Item)) &
      ": " & Outcome_Name (Item.Result));

   function Clock_Image (Time_Ms : Unsigned_64) return String is
      Milliseconds_Per_Second : constant := 1_000;
      Seconds_Per_Minute : constant := 60;
      Minutes_Per_Hour : constant := 60;
      Decimal_Base : constant := 10;
      Seconds : constant Unsigned_64 := Time_Ms / Milliseconds_Per_Second;
      Hours : constant Unsigned_64 := Seconds / (Seconds_Per_Minute * Minutes_Per_Hour);
      Minutes : constant Unsigned_64 := (Seconds / Seconds_Per_Minute) mod Minutes_Per_Hour;
      Rest : constant Unsigned_64 := Seconds mod Seconds_Per_Minute;
      function Two (Value : Unsigned_64) return String is
        ([Character'Val (Character'Pos ('0') + Natural (Value / Decimal_Base)),
          Character'Val (Character'Pos ('0') + Natural (Value mod Decimal_Base))]);
      Image : constant String := Hours'Image;
   begin
      return Image (Image'First + 1 .. Image'Last) & ":" & Two (Minutes) & ":" & Two (Rest);
   end Clock_Image;

   procedure Reset is
   begin
      if Entries /= null then
         Free (Entries);
      end if;
      First := 1;
      Used := 0;
      Total := 0;
   end Reset;
end Remote_Activity;
