pragma Ada_2022;
with System;
with System.Storage_Elements;
with CuBit.Channels;
with CuBit.Channel_Protocol;
with CuBit.Handles;
with CuBit.Handles.Startup;
with CuBit.Messages;
with CuBit.Monotonic;

package body CuBit.Metrics is
   use type Outlet.Series_Kind;
   use type Outlet.Unit;
   package Channels renames CuBit.Channels;
   use type Channels.Put_Result;
   use type Channels.Open_Result;

   --  The one bounded call that opens the ring: metrics.svc answers at once
   --  or the outlet counts as unwired, never stalling the first sample.
   Open_Wait_Ms : constant := 200;

   --  The series table: Capacity entries on the process heap, grown by
   --  doubling from Initial_Series.
   Initial_Series : constant := 16;
   type Series_State is record
      Name : Outlet.Name_Text := [others => ' '];
      Length : Outlet.Name_Length := 0;
      Kind : Series_Kind := Counter;
      Measure : Unit := Count;
      --  Its Describe is in the ring.
      Described : Boolean := False;
      --  Samples shed since the last Loss element went out, and when.
      Pending : Unsigned_64 := 0;
      Pending_First, Pending_Last : Unsigned_64 := 0;
      Lost : Unsigned_64 := 0;
   end record;
   type Series_Table is array (Outlet.Series_Key range <>) of Series_State;

   Table_Base : System.Address := System.Null_Address;
   Capacity : Outlet.Series_Count := 0;
   Used : Outlet.Series_Count := 0;

   Link : Channels.Channel;
   Current : Outlet_State := Not_Opened;
   Total_Lost, Total_Refused : Unsigned_64 := 0;
   --  Some series holds a loss not yet reported.
   Losses_Pending : Boolean := False;

   function CuAlloc_Allocate (Size, Alignment : Unsigned_64) return Unsigned_64
   with Import, Convention => C, External_Name => "cualloc_allocate";
   procedure CuAlloc_Free (Block : Unsigned_64)
   with Import, Convention => C, External_Name => "cualloc_free";
   No_Block : constant Unsigned_64 := 0;

   function Add (A, B : Unsigned_64) return Unsigned_64 is
     (if B > Unsigned_64'Last - A then Unsigned_64'Last else A + B);

   procedure Grow (Grown : out Boolean);
   procedure Grow (Grown : out Boolean) is
      use System.Storage_Elements;
      Wanted : constant Outlet.Series_Count :=
        (if Capacity = 0 then Initial_Series
         elsif Capacity > Outlet.Maximum_Series_Per_Source / 2 then Outlet.Maximum_Series_Per_Source
         else 2 * Capacity);
      Entry_Bytes : constant Unsigned_64 := Series_Table'Component_Size / System.Storage_Unit;
      Raw : Unsigned_64;
   begin
      Grown := False;
      if Wanted <= Capacity then
         return;
      end if;
      Raw := CuAlloc_Allocate (Unsigned_64 (Wanted) * Entry_Bytes, Series_State'Alignment);
      if Raw = No_Block then
         return;
      end if;
      declare
         Fresh : Series_Table (1 .. Wanted)
           with Import, Address => To_Address (Integer_Address (Raw));
      begin
         for I in Fresh'Range loop
            Fresh (I) := (others => <>);
         end loop;
         if Capacity > 0 then
            declare
               Old : constant Series_Table (1 .. Capacity) with Import, Address => Table_Base;
            begin
               Fresh (1 .. Capacity) := Old;
            end;
            CuAlloc_Free (Unsigned_64 (To_Integer (Table_Base)));
         end if;
      end;
      Table_Base := To_Address (Integer_Address (Raw));
      Capacity := Wanted;
      Grown := True;
   end Grow;

   function Series_Named (Name : String; Kind : Series_Kind; Measure : Unit) return Series is
      Grown : Boolean;
   begin
      if not Outlet.Valid_Name (Name) then
         Total_Refused := Add (Total_Refused, 1);
         return No_Series;
      end if;
      if Capacity > 0 then
         declare
            Table : constant Series_Table (1 .. Capacity) with Import, Address => Table_Base;
         begin
            for Key in 1 .. Used loop
               if Table (Key).Name (1 .. Table (Key).Length) = Name then
                  if Table (Key).Kind = Kind and then Table (Key).Measure = Measure then
                     return (Key => Key);
                  end if;
                  Total_Refused := Add (Total_Refused, 1);
                  return No_Series;
               end if;
            end loop;
         end;
      end if;
      if Used = Capacity then
         Grow (Grown);
         if not Grown then
            Total_Refused := Add (Total_Refused, 1);
            return No_Series;
         end if;
      end if;
      Used := Used + 1;
      declare
         Table : Series_Table (1 .. Capacity) with Import, Address => Table_Base;
      begin
         Table (Used).Name (1 .. Name'Length) := Name;
         Table (Used).Length := Name'Length;
         Table (Used).Kind := Kind;
         Table (Used).Measure := Measure;
      end;
      return (Key => Used);
   end Series_Named;

   --  Open the ring to metrics.svc, once: through the launcher's stdmetric
   --  entry, or Unwired when there is none or metrics.svc refuses.
   procedure Open_Outlet;
   procedure Open_Outlet is
      Target : constant CuBit.Handles.Startup.Optional_Endpoint :=
        CuBit.Handles.Startup.Optional_Endpoint_Named (Outlet.Startup_Binding);
      Result : Channels.Open_Result;
      Ignore_Refusal : CuBit.Channel_Protocol.Open_Refusal;
   begin
      Current := Unwired;
      if not Target.Present then
         return;
      end if;
      Channels.Open (Target.Value, Outlet.CONTRACT, Channels.Producing, Link, Result, Ignore_Refusal,
                     Deadline => CuBit.Messages.Deadline_After (Open_Wait_Ms));
      if Result = Channels.Opened then
         Current := Open;
      end if;
   end Open_Outlet;

   --  Put one element; Placed False when the ring shed it or is closed.
   procedure Put_Element (Item : Outlet.Element; Placed : out Boolean);
   procedure Put_Element (Item : Outlet.Element; Placed : out Boolean) is
      Bytes : Outlet.Element_Buffer;
      Length : Outlet.Element_Length;
      Result : Channels.Put_Result;
   begin
      Outlet.Encode (Item, Bytes, Length);
      Channels.Put (Link, Bytes'Address, Length, Result);
      Placed := Result = Channels.Put;
      if Result = Channels.Closed then
         --  metrics.svc ended: what follows is counted, never sent.
         Current := Unwired;
      end if;
   end Put_Element;

   --  The series' Describe and its pending Loss, if they are not in the
   --  ring yet. Ready False: one did not fit (the caller sheds).
   procedure Catch_Up (S : in out Series_State; Key : Outlet.Series_Key; Ready : out Boolean);
   procedure Catch_Up (S : in out Series_State; Key : Outlet.Series_Key; Ready : out Boolean) is
   begin
      Ready := True;
      if not S.Described then
         declare
            Describe : Outlet.Element (Outlet.Describe);
         begin
            Describe.Key := Key;
            Describe.Series := S.Kind;
            Describe.Measure := S.Measure;
            Describe.Length := S.Length;
            Describe.Name := S.Name;
            Put_Element (Describe, Ready);
         end;
         if not Ready then
            return;
         end if;
         S.Described := True;
      end if;
      if S.Pending > 0 then
         Put_Element ((Kind => Outlet.Loss, Key => Key, Shed => S.Pending,
                       First_Us => S.Pending_First, Last_Us => S.Pending_Last), Ready);
         if Ready then
            S.Pending := 0;
         end if;
      end if;
   end Catch_Up;

   procedure Shed (S : in out Series_State; Time_Us : Unsigned_64; Reported : Boolean);
   procedure Shed (S : in out Series_State; Time_Us : Unsigned_64; Reported : Boolean) is
   begin
      S.Lost := Add (S.Lost, 1);
      Total_Lost := Add (Total_Lost, 1);
      --  Only a wired outlet can report the loss; an unwired one counts it.
      if not Reported then
         return;
      end if;
      if S.Pending = 0 then
         S.Pending_First := Time_Us;
         S.Pending_Last := Time_Us;
      else
         S.Pending_First := Unsigned_64'Min (S.Pending_First, Time_Us);
         S.Pending_Last := Unsigned_64'Max (S.Pending_Last, Time_Us);
      end if;
      S.Pending := Add (S.Pending, 1);
      Losses_Pending := True;
   end Shed;

   procedure Observe_At (Item : Series; Value : Unsigned_64; Time_Us : Unsigned_64) is
      Ready, Placed : Boolean;
   begin
      if Item.Key = 0 or else Item.Key > Used then
         return;
      end if;
      if Current = Not_Opened then
         Open_Outlet;
      end if;
      declare
         Table : Series_Table (1 .. Capacity) with Import, Address => Table_Base;
         S : Series_State renames Table (Item.Key);
      begin
         if Current /= Open then
            Shed (S, Time_Us, Reported => False);
            return;
         end if;
         Catch_Up (S, Item.Key, Ready);
         if not Ready then
            Shed (S, Time_Us, Reported => Current = Open);
            return;
         end if;
         Put_Element ((Kind => Outlet.Sample, Key => Item.Key, Time_Us => Time_Us, Value => Value), Placed);
         if not Placed then
            Shed (S, Time_Us, Reported => Current = Open);
         end if;
      end;
   end Observe_At;

   procedure Note_Lost (Item : Series; Count, First_Us, Last_Us : Unsigned_64) is
   begin
      if Item.Key = 0 or else Item.Key > Used or else Count = 0 then
         return;
      end if;
      if Current = Not_Opened then
         Open_Outlet;
      end if;
      declare
         Table : Series_Table (1 .. Capacity) with Import, Address => Table_Base;
         S : Series_State renames Table (Item.Key);
      begin
         S.Lost := Add (S.Lost, Count);
         Total_Lost := Add (Total_Lost, Count);
         if Current /= Open then
            return;
         end if;
         if S.Pending = 0 then
            S.Pending_First := First_Us;
            S.Pending_Last := Last_Us;
         else
            S.Pending_First := Unsigned_64'Min (S.Pending_First, First_Us);
            S.Pending_Last := Unsigned_64'Max (S.Pending_Last, Last_Us);
         end if;
         S.Pending := Add (S.Pending, Count);
         Losses_Pending := True;
      end;
   end Note_Lost;

   procedure Observe (Item : Series; Value : Unsigned_64) is
   begin
      Observe_At (Item, Value, CuBit.Monotonic.Microseconds);
   end Observe;

   function Lost (Item : Series) return Unsigned_64 is
   begin
      if Item.Key = 0 or else Item.Key > Used then
         return 0;
      end if;
      declare
         Table : constant Series_Table (1 .. Capacity) with Import, Address => Table_Base;
      begin
         return Table (Item.Key).Lost;
      end;
   end Lost;

   function Lost return Unsigned_64 is (Total_Lost);
   function Refused return Unsigned_64 is (Total_Refused);
   function State return Outlet_State is (Current);

   procedure Report_Losses is
      Ready : Boolean;
   begin
      if not Losses_Pending or else Current /= Open or else Capacity = 0 then
         return;
      end if;
      Losses_Pending := False;
      declare
         Table : Series_Table (1 .. Capacity) with Import, Address => Table_Base;
      begin
         for Key in 1 .. Used loop
            if Table (Key).Pending > 0 then
               Catch_Up (Table (Key), Key, Ready);
               if Table (Key).Pending > 0 then
                  Losses_Pending := True;
               end if;
            end if;
         end loop;
      end;
   end Report_Losses;

   procedure Flush (Drained : out Boolean; Wait_Ms : Natural := Default_Flush_Wait_Ms) is
      Waited : Natural := 0;
      Ignore : Unsigned_64;
   begin
      Drained := False;
      if Current /= Open then
         return;
      end if;
      Report_Losses;
      if Channels.All_Taken (Link) then
         Drained := True;
         return;
      end if;
      Channels.Kick (Link);
      while Waited < Wait_Ms loop
         if Channels.All_Taken (Link) then
            Drained := True;
            return;
         end if;
         Ignore := CuBit.Messages.syscall (CuBit.Messages.SYSCALL_SLEEP, 1);
         Waited := Waited + 1;
      end loop;
   end Flush;
end CuBit.Metrics;
