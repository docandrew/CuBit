--  Hosted tests of the driver-budget log issuances (ABI-002): every
--  publisher's tag is unique.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Log_Protocol;
with Driver_Log_Issuances; use Driver_Log_Issuances;

procedure Log_Issuances_Tests is
   Failures, Checks : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;
   A : Allocator;
   Seen : array (1 .. 4096) of Unsigned_64 := [others => 0];
   V : Unsigned_64;
   Granted : Boolean;
begin
   Check (Fixed_Unique, "fixed issuances distinct and below the allocated range");
   --  Tags, not just issuances: distinct publishers, distinct tags.
   for P in Fixed_Publisher loop
      for Q in Fixed_Publisher loop
         if P /= Q then
            Check (CuBit.Log_Protocol.Publisher_Tag (Budget, Fixed (P)) /=
                   CuBit.Log_Protocol.Publisher_Tag (Budget, Fixed (Q)),
                   "tags of " & P'Image & " and " & Q'Image);
         end if;
      end loop;
   end loop;
   --  The launchers' own warnings no longer share a driver's issuance.
   Check (Fixed (Device_Manager) /= Fixed (Netmgr) and then Fixed (Process_Manager) /= Fixed (Wifi),
          "devmgr and procmgr have their own issuances");
   for I in Seen'Range loop
      Allocate (A, V, Granted);
      Check (Granted and then V >= First_Allocated, "allocated above the fixed table");
      for P in Fixed_Publisher loop
         Check (V /= Fixed (P), "allocated never a fixed one");
      end loop;
      Seen (I) := V;
   end loop;
   for I in Seen'Range loop
      for J in I + 1 .. Seen'Last loop
         if Seen (I) = Seen (J) then
            Check (False, "issuance handed out twice");
         end if;
      end loop;
   end loop;
   --  Exhaustion: nothing granted, nothing reused.
   A.Next := CuBit.Log_Protocol.Publication_Issuance'Last;
   Allocate (A, V, Granted);
   Check (Granted and then V = CuBit.Log_Protocol.Publication_Issuance'Last, "the last issuance");
   Allocate (A, V, Granted);
   Check (not Granted, "none past the last");
   Put_Line ("log-issuances:" & Checks'Image & " checks," & Failures'Image & " failures");
   if Failures /= 0 then
      raise Program_Error;
   end if;
end Log_Issuances_Tests;
