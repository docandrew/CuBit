--  Volume name selection: registered volumes, the fixed "@boot" and
--  "@cd:0" stores, unqualified, unknown and invalid names.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Volume_List; use Volume_List;

procedure Main is
   Failures : Natural := 0;
   Checks   : Natural := 0;
   List     : State;
   Volume   : Volume_Reference;
   Result   : Registration_Result;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   procedure Expect (Path : String; Selection : Path_Selection;
                     Volume : Volume_Reference; Relative : String) is
      Got : Path_Selection;
      Got_Volume : Volume_Reference;
      First : Integer;
   begin
      Select_Path (List, Path, Got, Got_Volume, First);
      Check (Got = Selection, Path & ": " & Got'Image);
      Check (Got_Volume = Volume, Path & ": volume" & Got_Volume'Image);
      if Got in Known_Volume | Boot_Archive | Optical_Volume | Unqualified then
         Check (Path (First .. Path'Last) = Relative,
                Path & ": relative """ & Path (First .. Path'Last) & """");
      end if;
   end Expect;
begin
   Register (List, "nvme:0", (Endpoint => 3, Ready_Role => 0, Transfer_Pages => 1),
             Volume, Result);
   Check (Result = Registered and Volume = 1, "nvme:0 registered");
   Register (List, "boot", (Endpoint => 4, Ready_Role => 0, Transfer_Pages => 1),
             Volume, Result);
   Check (Result = Invalid_Name and Volume = No_Volume, "boot is reserved");
   Register (List, "cd:0", (Endpoint => 5, Ready_Role => 0, Transfer_Pages => 1),
             Volume, Result);
   Check (Result = Invalid_Name, "cd:0 is reserved");
   Register (List, "cd:1", (Endpoint => 6, Ready_Role => 0, Transfer_Pages => 1),
             Volume, Result);
   Check (Result = Registered and Volume = 2, "cd:1 is an ordinary name");

   Expect ("@nvme:0/sameboy/00.gb", Known_Volume, 1, "sameboy/00.gb");
   Expect ("@boot/doom1.wad", Boot_Archive, No_Volume, "doom1.wad");
   Expect ("@boot", Boot_Archive, No_Volume, "");
   Expect ("@boot/", Boot_Archive, No_Volume, "");
   Expect ("@cd:0/apps/doom1.wad", Optical_Volume, No_Volume, "apps/doom1.wad");
   Expect ("@cd:1/x", Known_Volume, 2, "x");
   Expect ("@bootx/doom1.wad", Unknown_Volume, No_Volume, "");
   Expect ("@boo/doom1.wad", Unknown_Volume, No_Volume, "");
   Expect ("@cd:00/x", Unknown_Volume, No_Volume, "");
   Expect ("@/x", Invalid_Path, No_Volume, "");
   Expect ("doom1.wad", Unqualified, No_Volume, "doom1.wad");
   Expect ("apps/doom1.wad", Unqualified, No_Volume, "apps/doom1.wad");

   if Failures = 0 then
      Put_Line ("VOLUME-NAMES: PASS" & Checks'Image & " checks");
   else
      Put_Line ("VOLUME-NAMES: FAIL" & Failures'Image & " of" & Checks'Image);
      raise Program_Error;
   end if;
end Main;
