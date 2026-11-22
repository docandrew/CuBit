with Ada.Text_IO;
with Interfaces; use Interfaces;
with Grant_Page_Installation;

procedure Mapping_Test is
   subtype Page_Index is Natural range 0 .. 4095;
   type Counts is array (Page_Index) of Natural;
   type Mappings is array (Page_Index) of Boolean;
   Pins : Counts := [others => 1]; -- unrelated parent mapping owns one pin
   Mapped : Mappings := [others => False];
   Same_Physical_Frame : Boolean := False;
   Fail_Resolve, Fail_Install : Natural := 4096;
   Resolved, Installed_Calls, Retired, Released : Natural := 0;
   Installed : Natural;
   OK : Boolean;
   Pending : Boolean := False;
   Pending_Frame : Unsigned_64 := 0;

   function Frame (Page : Natural) return Unsigned_64 is
     (if Same_Physical_Frame then 4096 else Unsigned_64 (Page + 1) * 4096);
   function Index (Physical : Unsigned_64) return Page_Index is
     (Page_Index (Physical / 4096 - 1));

   procedure Resolve
     (Page : Natural; Physical : out Unsigned_64; Success : out Boolean)
   is
   begin
      pragma Assert (not Pending and Page = Resolved);
      Resolved := Resolved + 1;
      Physical := Frame (Page);
      Success := Page /= Fail_Resolve;
      if Success then
         Pins (Index (Physical)) := Pins (Index (Physical)) + 1;
         Pending := True;
         Pending_Frame := Physical;
      end if;
   end Resolve;

   procedure Install
     (Page : Natural; Physical : Unsigned_64; Success : out Boolean)
   is
   begin
      pragma Assert (Pending and Physical = Pending_Frame
                     and Physical = Frame (Page) and not Mapped (Page));
      Installed_Calls := Installed_Calls + 1;
      Success := Page /= Fail_Install;
      if Success then
         Mapped (Page) := True;
         Pending := False;
      end if;
   end Install;

   procedure Release_Unpublished (Physical : Unsigned_64) is
   begin
      pragma Assert (Pending and Physical = Pending_Frame);
      pragma Assert (Pins (Index (Physical)) > 1);
      Pins (Index (Physical)) := Pins (Index (Physical)) - 1;
      Pending := False;
      Released := Released + 1;
   end Release_Unpublished;

   procedure Retire (Pages : Positive) is
   begin
      pragma Assert (not Pending and Retired = 0);
      Retired := Retired + 1;
      for Page in 0 .. Pages - 1 loop
         pragma Assert (Mapped (Page));
         Mapped (Page) := False;
      end loop;
      -- Simulated acknowledged shootdown. Production callback performs it;
      -- this test covers transaction callback order, not physical CPU TLBs.
      for Page in 0 .. Pages - 1 loop
         pragma Assert (not Mapped (Page) and Pins (Index (Frame (Page))) > 1);
         Pins (Index (Frame (Page))) := Pins (Index (Frame (Page))) - 1;
      end loop;
   end Retire;

   procedure Run is new Grant_Page_Installation
     (Unsigned_64, Resolve, Install, Release_Unpublished, Retire);

   procedure Check (Pages : Positive; Failure : Natural; At_Install : Boolean) is
   begin
      Pins := [others => 1];
      Mapped := [others => False];
      Pending := False;
      Resolved := 0;
      Installed_Calls := 0;
      Retired := 0;
      Released := 0;
      Fail_Resolve := (if At_Install then 4096 else Failure);
      Fail_Install := (if At_Install then Failure else 4096);
      Installed := 999;
      Run (Pages, Installed, OK);
      pragma Assert (not Pending);
      if Failure < Pages then
         pragma Assert (not OK and Installed = 0 and Resolved = Failure + 1);
         pragma Assert (Installed_Calls = Failure + Boolean'Pos (At_Install));
         pragma Assert (Released = Boolean'Pos (At_Install));
         pragma Assert (Retired = Boolean'Pos (Failure > 0));
      else
         pragma Assert (OK and Installed = Pages and Resolved = Pages
                        and Installed_Calls = Pages and Retired = 0
                        and Released = 0);
         if Same_Physical_Frame then
            pragma Assert (Pins (0) = Pages + 1);
         else
            for Page in 0 .. Pages - 1 loop
               pragma Assert (Pins (Page) = 2 and Mapped (Page));
            end loop;
         end if;
         Retire (Pages);
      end if;
      pragma Assert (Pins = Counts'(others => 1));
      pragma Assert (Mapped = Mappings'(others => False));
   end Check;
begin
   for Same_Frame in Boolean loop
      Same_Physical_Frame := Same_Frame;
      for Pages in 1 .. 8 loop
         for Failure in 0 .. Pages loop
            Check (Pages, Failure, False);
            Check (Pages, Failure, True);
         end loop;
      end loop;
      Check (4096, 4096, False);
      Check (4096, 4095, False);
      Check (4096, 4095, True);
   end loop;
   Ada.Text_IO.Put_Line
     ("PASS grant mapping transaction: pin/map failures, rollback, aliases, 4096 pages (hosted callbacks)");
end Mapping_Test;
