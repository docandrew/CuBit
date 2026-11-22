------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Input_Rings;

package body Input_Source_Set is

   package Channels renames CuBit.Input_Rings.Channels;

   type Ring_State is limited record
      Used   : Boolean := False;
      Source : Unsigned_64 := 0;
      Ring   : Channels.Consumer;
   end record;
   Rings : array (Slot_Number) of Ring_State;
   Next_Source : Unsigned_64 := Ring_Source_Base;
   Active_Count : Natural := 0;

   function Free_Slot return Natural is
   begin
      for S in Slot_Number loop
         if not Rings (S).Used then
            return S;
         end if;
      end loop;
      return 0;
   end Free_Slot;

   procedure Add
     (Slot : Slot_Number; Own_Region, Producer_Region : System.Address; Source : out Unsigned_64) is
   begin
      Source := 0;
      if Next_Source = Unsigned_64'Last then
         return;
      end if;
      Next_Source := Next_Source + 1;
      Source := Next_Source;
      Rings (Slot).Used := True;
      Rings (Slot).Source := Source;
      Channels.Attach_Consumer (Rings (Slot).Ring, Own_Region, Producer_Region);
      Active_Count := Active_Count + 1;
   end Add;

   procedure Drain_Slot (Slot : Slot_Number) is
      Head : CuBit.Input.Report_Words;
      Got, Ring_Peer : Boolean;
   begin
      if not Rings (Slot).Used then
         return;
      end if;
      --  At most a ring's worth: the producer is gone or going.
      for N in 1 .. Channels.Rings.Slots loop
         Channels.Take (Rings (Slot).Ring, Head, Got, Ring_Peer);
         exit when not Got;
         Deliver (Head, Rings (Slot).Source);
      end loop;
   end Drain_Slot;

   procedure Remove (Slot : Slot_Number) is
   begin
      if Rings (Slot).Used then
         Channels.Detach_Consumer (Rings (Slot).Ring);
         Rings (Slot).Used := False;
         Rings (Slot).Source := 0;
         Active_Count := Active_Count - 1;
      end if;
   end Remove;

   function Active (Slot : Slot_Number) return Boolean is (Rings (Slot).Used);
   function Source_Of (Slot : Slot_Number) return Unsigned_64 is (Rings (Slot).Source);
   function Count return Natural is (Active_Count);

   --  When a report was acquired (unstamped reports sort after stamped
   --  ones, in ring order).
   function Stamp (Words : CuBit.Input.Report_Words) return Unsigned_64 is
     (CuBit.Input.Source_Time_Us (CuBit.Input.To_Message (Words)));

   procedure Drain (Budget : Positive; Delivered : out Natural) is
      Head : CuBit.Input.Report_Words;
      Got, Ring_Peer : Boolean;
      Oldest : Natural;
      Oldest_Stamp, This_Stamp : Unsigned_64;
   begin
      Delivered := 0;
      if Active_Count = 0 then
         return;
      end if;
      while Delivered < Budget loop
         Oldest := 0;
         Oldest_Stamp := Unsigned_64'Last;
         for S in Slot_Number loop
            if Rings (S).Used then
               Channels.Peek (Rings (S).Ring, Head, Got);
               if Got then
                  This_Stamp := Stamp (Head);
                  if Oldest = 0 or else This_Stamp < Oldest_Stamp then
                     Oldest := S;
                     Oldest_Stamp := This_Stamp;
                  end if;
               end if;
            end if;
         end loop;
         exit when Oldest = 0;
         --  The copy Take makes is the report delivered.
         Channels.Take (Rings (Oldest).Ring, Head, Got, Ring_Peer);
         exit when not Got;
         Delivered := Delivered + 1;
         if Ring_Peer then
            Wake_Producer (Oldest);
         end if;
         Deliver (Head, Rings (Oldest).Source);
      end loop;
   end Drain;

   function Quiet return Boolean is
   begin
      for S in Slot_Number loop
         if Rings (S).Used and then not Channels.Arm (Rings (S).Ring) then
            return False;
         end if;
      end loop;
      return True;
   end Quiet;

   function Waiting return Boolean is
   begin
      for S in Slot_Number loop
         if Rings (S).Used and then Channels.Can_Take (Rings (S).Ring) then
            return True;
         end if;
      end loop;
      return False;
   end Waiting;

   procedure Disarm_All is
   begin
      for S in Slot_Number loop
         if Rings (S).Used then
            Channels.Disarm (Rings (S).Ring);
         end if;
      end loop;
   end Disarm_All;

   function Producer_Overflows (Slot : Slot_Number) return Unsigned_32 is
     (if Rings (Slot).Used then Channels.Peer_Overflows (Rings (Slot).Ring) else 0);

end Input_Source_Set;
