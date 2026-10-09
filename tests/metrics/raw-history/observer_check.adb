with Interfaces; use Interfaces;
with Ada.Text_IO;
with Control; use type Control.Mode;
with CuBit.Metric_Raw_Observer;
with CuBit.Metric_Protocol;
procedure Check is
 package O renames CuBit.Metric_Raw_Observer;
 package P renames CuBit.Metric_Protocol;
 use type P.Status;
 Page : P.Raw_Page;
 Written : P.Raw_Row_Count;
 Next, Gap, Dropped : Unsigned_64;
 Result : P.Status;
 Done : Boolean;
 Checks : Natural := 0;
 procedure Require (V : Boolean) is
 begin Checks := Checks + 1; if not V then raise Program_Error; end if; end;
begin
 for Kind in Control.Mode loop
  declare Item : O.Observer (1); Before : Natural;
  begin
   Control.Current := Kind; Control.Identity := 2 ** 32 + 77;
   Control.Retired := False; Control.Create_OK := True; Control.Revoke_OK := False;
   O.Query (Item, 1, Page, Written, Next, Gap, Dropped, Result);
   if Kind = Control.Success then
    Require (Result = P.OK and Written = 1 and Next = 2 and not O.Disabled (Item));
    Control.Identity := Control.Identity + 2 ** 32;
   else Require (Result /= P.OK and Written = 0 and Next = 1 and O.Disabled (Item)); end if;
   Before := Control.Calls;
   O.Query (Item, 2, Page, Written, Next, Gap, Dropped, Result);
   Require (O.Disabled (Item) and Control.Calls = Before);
   O.Disconnect (Item, Done); Require (not Done);
   Control.Retired := True; O.Disconnect (Item, Done); Require (Done);
  end;
 end loop;
 declare Item : O.Observer (1); Before : constant Natural := Control.Calls;
 begin
  Control.Current := Control.Success; Control.Identity := 2 ** 32 + 77;
  O.Query (Item, 0, Page, Written, Next, Gap, Dropped, Result);
  Require (Result = P.Invalid_Request and not O.Disabled (Item) and Control.Calls = Before);
  Control.Create_OK := False;
  O.Query (Item, 1, Page, Written, Next, Gap, Dropped, Result);
  Require (Result = P.Unavailable and not O.Disabled (Item) and Control.Calls = Before);
  Control.Create_OK := True;
  O.Query (Item, 1, Page, Written, Next, Gap, Dropped, Result);
  Require (Result = P.OK and O.Incarnation (Item) = Control.Identity);
  O.Disconnect (Item, Done); Require (Done);
 end;
 declare Item : O.Observer (1); Before : constant Natural := Control.Calls;
 begin
  Control.Identity := 0;
  O.Query (Item, 1, Page, Written, Next, Gap, Dropped, Result);
  Require (Result = P.Unavailable and O.Disabled (Item) and Control.Calls = Before);
  O.Disconnect (Item, Done); Require (Done);
 end;
 Ada.Text_IO.Put_Line ("PASS raw-observer boundary checks:" & Checks'Image);
end Check;
