with Ada.Text_IO;
with Windowed_Reads;

procedure Read_Tests is
   Window : constant := 1024 * 1024;
   type Fault is (None, Short_Read, Oversized_Reply, Read_Error, Retirement_Error);
   Mode : Fault := None;
   Fail_At, Calls, Seen, Length, Cases : Natural := 0;
   procedure Transfer
     (Offset : Natural; Count : Positive;
      Transferred : out Natural; Success : out Boolean)
   is
   begin
      pragma Assert (Offset = Seen and Offset mod Window = 0);
      pragma Assert (Count <= Window and Count <= Length - Offset);
      pragma Assert (Count = Window or else Offset + Count = Length);
      Calls := Calls + 1;
      Transferred := Count;
      Success := True;
      if Calls = Fail_At then
         case Mode is
            when None => null;
            when Short_Read => Transferred := Count - 1;
            when Oversized_Reply => Transferred := Count + 1;
            when Read_Error | Retirement_Error => Success := False;
         end case;
      end if;
      Seen := Seen + Count;
   end Transfer;
   package Reader is new Windowed_Reads (Window, Transfer);
   Sizes : constant array (Positive range <>) of Natural :=
     [0, 1, 4095, 4096, Window - 1, Window, Window + 1,
      16 * Window, 18 * Window + 71, Natural'Last];
begin
   for Size of Sizes loop
      Length := Size;
      declare
         Expected : constant Natural := Size / Window + Boolean'Pos (Size mod Window /= 0);
         Points : constant array (1 .. 3) of Natural := [1, Expected / 2 + 1, Expected];
      begin
         for M in Fault loop
            Mode := M;
            for Point of Points loop
               Fail_At := Point;
               Calls := 0;
               Seen := 0;
               declare
                  Result : constant Boolean := Reader.Read_All (Size);
               begin
                  if Size = 0 then
                     pragma Assert (not Result and Calls = 0);
                  elsif Mode = None then
                     pragma Assert (Result and Seen = Size and Calls = Expected);
                  else
                     pragma Assert (not Result and Calls = Point);
                  end if;
               end;
               Cases := Cases + 1;
            end loop;
         end loop;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("PASS exact file-window sequencing/failure cases:" & Cases'Image);
end Read_Tests;
