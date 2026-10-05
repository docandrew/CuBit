with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Mesa_Service; use Mesa_Service;
with Ada.Text_IO;
procedure Service_Ada_Tests is
   procedure Reset (Mode : Unsigned_32)
     with Import, Convention => C, External_Name => "fixture_reset";
   function Starts return Unsigned_32
     with Import, Convention => C, External_Name => "fixture_starts";
   function Borrows return Unsigned_32
     with Import, Convention => C, External_Name => "fixture_borrows";
   function Closes return Unsigned_32
     with Import, Convention => C, External_Name => "fixture_closes";
   procedure Check (Value : Boolean) is
   begin
      if not Value then raise Program_Error with "Mesa Ada boundary oracle"; end if;
   end;
begin
   for Mode in Unsigned_32 range 0 .. 2 loop
      declare
         Object : Owner;
         Code : Result;
         View : Device_View;
         Ready : Boolean;
      begin
         Reset (Mode);
         Check (not Accepted (Object));
         Check (Health (Object) = Initialization_Failed);
         Check (Close (Object, True) = Unsafe);
         Borrow (Object, View, Ready);
         Check (not Ready and View.Device = Null_Address and Borrows = 0);
         Start (Object, 7, Code);
         Check (Starts = 1);
         Check (Code = (if Mode = 0 then 0 elsif Mode = 1 then -3 else -2));
         Check (Accepted (Object) = (Mode /= 1));
         Start (Object, 7, Code);
         Check (Starts = 1 and Code = Initialization_Failed);
         Borrow (Object, View, Ready);
         if Mode = 0 then
            Check (Ready and View.Instance = To_Address (11) and
              View.Physical = To_Address (22) and View.Device = To_Address (33)
              and View.Queue = To_Address (44) and View.Family = 55 and
              View.Instance_Proc /= Null_Address);
         else
            Check (not Ready and View = (Null_Address, Null_Address,
              Null_Address, Null_Address, 0, Null_Address));
         end if;
         if Mode /= 1 then
            Check (Health (Object) = -4);
            Check (Close (Object, False) = Pending and Closes = 0);
            Check (Close (Object, True) = (if Mode = 0 then Pending else Unsafe));
            Check (Closes = 1);
            Borrow (Object, View, Ready);
            Check (not Ready and View.Device = Null_Address and Borrows = 1);
            Check (Health (Object) = Initialization_Failed);
            Check (Close (Object, True) = (if Mode = 0 then Retired else Unsafe));
            Check (Closes = 2 and Accepted (Object));
            Start (Object, 7, Code);
            Check (Starts = 1 and Code = Initialization_Failed);
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("PASS: Ada/C layout, three lifecycle paths, retained failure and close gating");
end Service_Ada_Tests;
