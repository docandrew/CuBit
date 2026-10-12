pragma Ada_2022;
with System;
with BuddyAllocator;
with Physical_Range_Map;
with Physical_Ranges;
with Process_Identities;
with Publication_Pages;
with Spinlocks;
with TextIO; use TextIO;
with Time;
with Util;
with Virtmem;

package body Device_Memory_Diagnostics with SPARK_Mode => Off is

   package Pub renames Device_Memory_Publication;
   package PR renames Physical_Ranges;
   use type Virtmem.PhysAddress;

   Frame : Virtmem.PhysAddress := 0;
   Lock  : Spinlocks.Spinlock;
   -- Kills are counted even before the page exists (System facts).
   Kills     : Unsigned_64 := 0;
   Kill_Lock : Spinlocks.Spinlock;

   procedure Publish_Classification is
      Ignore : System.Address;
      Runs   : PR.Run_Array;
      Count  : PR.Root_Count;
      M      : PR.Map renames Physical_Range_Map.Kernel_Map;
   begin
      if Frame /= 0 then
         return;
      end if;
      BuddyAllocator.allocFrame (Frame);
      if Frame = 0 then
         raise Program_Error with "No frame for device-memory diagnostics";
      end if;
      Ignore := Util.memset (Virtmem.P2Va (Frame), 0, Pub.Page_Bytes);
      declare
         Page : Pub.Diagnostics_Page with
           Import, Volatile, Address => Virtmem.P2Va (Frame);
      begin
         Page.Span_Count := Unsigned_64 (M.Count);
         Page.Complete := (if M.Complete and then M.Sealed then 1 else 0);
         for I in 1 .. Natural'Min (M.Count, Pub.Span_Capacity) loop
            Page.Spans (I) :=
              (First => M.Spans (I).First, Last => M.Spans (I).Last,
               Class => Unsigned_64 (PR.Range_Class'Pos (M.Spans (I).Class)));
         end loop;
         PR.Largest_Device_Runs (M, Runs, Count);
         Page.Root_Count := Unsigned_64 (Count);
         for I in 1 .. Count loop
            Page.Roots (I) := (First => Runs (I).First, Last => Runs (I).Last);
         end loop;
         Page.Version := Pub.Layout_Version;
      end;
   end Publish_Classification;

   procedure Record_Denial
     (Caller  : Process.ProcessID;
      Request : Device_Memory_Publication.Operation;
      Base    : Unsigned_64;
      Bytes   : Unsigned_64;
      Mode    : Device_Memory_Authority.Access_Mode;
      Why     : Device_Memory_Authority.Explanation)
   is
      use type Pub.Reason;
      Identity : constant Unsigned_64 :=
        Process_Identities.To_Word (Process.identityOf (Caller));
      Name : constant Pub.Process_Name := Process.proctab (Caller).name;
      Class : constant Unsigned_64 := Unsigned_64 (PR.Range_Class'Pos (Why.Class));
      -- Device_Memory_Admission and the publication number modes alike.
      Mode_Word : constant Unsigned_64 :=
        Unsigned_64 (Device_Memory_Authority.Access_Mode'Pos (Mode));
   begin
      print ("device-memory denied: ");
      print (Name);
      print (" ");
      print (Pub.Operation_Name (Unsigned_64 (Pub.Operation'Pos (Request))));
      print (" base ");
      print (Base);
      print (" bytes ");
      print (Bytes);
      print (" ");
      print (Pub.Mode_Name (Mode_Word));
      print (": ");
      print (Pub.Reason_Name (Unsigned_64 (Pub.Reason'Pos (Why.Why))));
      if Why.Why = Pub.Blocked_Page then
         print (" page ");
         print (Why.Page);
         print (" ");
         print (Pub.Class_Name (Class));
      end if;
      println;
      if Frame = 0 then
         return;
      end if;
      Spinlocks.enterCriticalSection (Lock);
      declare
         Page : Pub.Diagnostics_Page with
           Import, Volatile, Address => Virtmem.P2Va (Frame);
         Number : constant Unsigned_64 :=
           (if Page.Total = Unsigned_64'Last then Page.Total else Page.Total + 1);
         Slot : constant Pub.Denial_Slot := Pub.Slot_Of (Number);
         Sequence : Unsigned_64 with
           Import, Atomic, Address => Page.Denials (Slot).Sequence'Address;
         Total : Unsigned_64 with
           Import, Atomic, Address => Page.Total'Address;
      begin
         -- Readers see Writing until every field is in place.
         Sequence := Pub.Writing;
         Page.Denials (Slot).Caller  := Identity;
         Page.Denials (Slot).Name    := Name;
         Page.Denials (Slot).Request := Unsigned_64 (Pub.Operation'Pos (Request));
         Page.Denials (Slot).Why     := Unsigned_64 (Pub.Reason'Pos (Why.Why));
         Page.Denials (Slot).Base    := Base;
         Page.Denials (Slot).Bytes   := Bytes;
         Page.Denials (Slot).Page    := Why.Page;
         Page.Denials (Slot).Class   := Class;
         Page.Denials (Slot).Mode    := Mode_Word;
         Sequence := Number;
         Total := Number;
         Publication_Pages.Set_Device_Memory_Denials (Number);
      end;
      Spinlocks.exitCriticalSection (Lock);
   end Record_Denial;

   procedure Record_Kill
     (Victim  : Process.ProcessID;
      Kind    : Device_Memory_Publication.Kill_Kind;
      Vector  : Unsigned_64;
      Address : Unsigned_64;
      RIP     : Unsigned_64)
   is
      Identity : constant Unsigned_64 :=
        Process_Identities.To_Word (Process.identityOf (Victim));
      Name : constant Pub.Process_Name := Process.proctab (Victim).name;
      Now  : constant Unsigned_64 := Unsigned_64 (Time.msTicks);
   begin
      print ("process killed: ");
      print (Name);
      print (" ");
      print (Pub.Kill_Kind_Name (Unsigned_64 (Pub.Kill_Kind'Pos (Kind))));
      print (" vector ");
      print (Vector);
      print (" address ");
      print (Address);
      print (" rip ");
      println (RIP);
      Spinlocks.enterCriticalSection (Kill_Lock);
      Kills := (if Kills = Unsigned_64'Last then Kills else Kills + 1);
      Publication_Pages.Set_Process_Kills (Kills);
      if Frame /= 0 then
         declare
            Page : Pub.Diagnostics_Page with
              Import, Volatile, Address => Virtmem.P2Va (Frame);
            Slot : constant Pub.Kill_Slot := Pub.Kill_Slot_Of (Kills);
            Sequence : Unsigned_64 with
              Import, Atomic, Address => Page.Kills (Slot).Sequence'Address;
            Total : Unsigned_64 with
              Import, Atomic, Address => Page.Kill_Total'Address;
         begin
            -- Readers see Writing until every field is in place.
            Sequence := Pub.Writing;
            Page.Kills (Slot).Victim  := Identity;
            Page.Kills (Slot).Name    := Name;
            Page.Kills (Slot).Kind    := Unsigned_64 (Pub.Kill_Kind'Pos (Kind));
            Page.Kills (Slot).Vector  := Vector;
            Page.Kills (Slot).Address := Address;
            Page.Kills (Slot).RIP     := RIP;
            Page.Kills (Slot).Time_Ms := Now;
            Sequence := Kills;
            Total := Kills;
         end;
      end if;
      Spinlocks.exitCriticalSection (Kill_Lock);
   end Record_Kill;

   procedure Map_Into (Reader : Process.ProcessID; Mapped : out Boolean) is
      procedure mapPage is new Virtmem.mapPage (BuddyAllocator.allocFrame);
   begin
      Mapped := False;
      if Frame = 0 then
         return;
      end if;
      Process.lockAddressSpace (Reader);
      if Virtmem.tableWalk (Pub.Page_Address, Process.addrtab (Reader)) =
        Virtmem.PhysAddress (Frame)
      then
         Mapped := True;   -- registered again: already readable
      else
         mapPage (Frame, Pub.Page_Address, Virtmem.PG_USERDATARO,
                  Process.addrtab (Reader), Mapped);
      end if;
      Process.unlockAddressSpace (Reader);
   end Map_Into;

end Device_Memory_Diagnostics;
