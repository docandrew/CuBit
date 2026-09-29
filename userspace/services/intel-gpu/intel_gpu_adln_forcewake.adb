with Intel_GPU_Forcewake;
with Intel_GPU_Domain_Lease;
package body Intel_GPU_ADLN_Forcewake is
   use Intel_GPU_ADLN_Inventory;
   Failed : Boolean := False;
   Failed_Domain : Domain := GT;
   type Reason is (Timeout, Poll_Exhausted, Invalid_MMIO, Invalid_Clock, Invalid_State);
   Failed_Status : Reason := Timeout;
   Failed_Release : Boolean := False;
   Last_Ack : Interfaces.Unsigned_32 := 0;
   function Failure_Detail return String is
      function Hex (Value : Interfaces.Unsigned_32) return String is
         use Interfaces;
         Hex_Digits : constant String := "0123456789ABCDEF";
         Text : String (1 .. 8);
         Remaining : Unsigned_32 := Value;
      begin
         for I in reverse Text'Range loop
            Text (I) := Hex_Digits (Natural (Remaining and 15) + 1);
            Remaining := Shift_Right (Remaining, 4);
         end loop;
         return Text;
      end Hex;
   begin
      if not Failed then return "none"; end if;
      return "domain=" & Natural'Image (Domain'Pos (Failed_Domain)) &
        (if Failed_Release then " release " else " acquire ") &
        (case Failed_Status is
           when Timeout => "deadline-expired",
           when Poll_Exhausted => "poll-budget-exhausted",
           when Invalid_MMIO => "invalid-MMIO",
           when Invalid_Clock => "invalid-clock",
           when Invalid_State => "invalid-state") & " ack=" & Hex (Last_Ack);
   end Failure_Detail;
   generic
      Item : Domain;
   package Binding is
      procedure Acquire (Success : out Boolean);
      procedure Release (Success : out Boolean);
   end Binding;
   package body Binding is
      procedure Recover_Ack (Expected : Interfaces.Unsigned_32; Recovered : in out Boolean) is
      begin Recover_Domain (Item, Expected, Recovered); end Recover_Ack;
      Observed_Ack : Interfaces.Unsigned_32 := 0;
      function Read_Ack (Offset : Interfaces.Unsigned_32)
        return Interfaces.Unsigned_32 is
      begin
         Observed_Ack := Read_32 (Offset);
         return Observed_Ack;
      end Read_Ack;
      package FW is new Intel_GPU_Forcewake
        (Read_Ack, Write_32, Pause, Now_Milliseconds,
         Request_Register (Item), Ack_Register (Item), Recover_Ack);
      Object : FW.Lease;
      use type FW.Result;
      procedure Record_Failure (Status : FW.Result; Releasing : Boolean) is
      begin
         if Status = FW.Ready or Failed then return; end if;
         Failed := True;
         Failed_Domain := Item;
         Failed_Release := Releasing;
         Last_Ack := Observed_Ack;
         Failed_Status := (case Status is
            when FW.Timed_Out => Timeout,
            when FW.Poll_Exhausted => Poll_Exhausted,
            when FW.Invalid_MMIO => Invalid_MMIO,
            when FW.Invalid_Clock => Invalid_Clock,
            when FW.Invalid_State => Invalid_State,
            when FW.Ready => Timeout);
      end Record_Failure;
      procedure Acquire (Success : out Boolean) is
         Status : FW.Result;
      begin
         FW.Acquire (Object, Poll_Limit, Status);
         Record_Failure (Status, False);
         Success := Status = FW.Ready;
      end Acquire;
      procedure Release (Success : out Boolean) is
         Status : FW.Result;
      begin
         FW.Release (Object, Poll_Limit, Status);
         Record_Failure (Status, True);
         Success := Status = FW.Ready;
      end Release;
   end Binding;
   package D0 is new Binding (GT);
   package D1 is new Binding (Render_Domain);
   package D2 is new Binding (VDBOX_0);
   package D3 is new Binding (VDBOX_2);
   package D4 is new Binding (VEBOX_0);
   procedure Acquire_One (Item : Domain; Success : out Boolean) is
   begin
      case Item is
         when GT => D0.Acquire (Success);
         when Render_Domain => D1.Acquire (Success);
         when VDBOX_0 => D2.Acquire (Success);
         when VDBOX_2 => D3.Acquire (Success);
         when VEBOX_0 => D4.Acquire (Success);
      end case;
   end Acquire_One;
   procedure Release_One (Item : Domain; Success : out Boolean) is
   begin
      case Item is
         when GT => D0.Release (Success);
         when Render_Domain => D1.Release (Success);
         when VDBOX_0 => D2.Release (Success);
         when VDBOX_2 => D3.Release (Success);
         when VEBOX_0 => D4.Release (Success);
      end case;
   end Release_One;
   package Coordinator is new Intel_GPU_Domain_Lease (Domain, Acquire_One, Release_One);
   Object : Coordinator.Lease;
   function State return Ownership_State is
     (case Coordinator.State (Object) is
        when Coordinator.Idle => Idle, when Coordinator.Held => Held,
        when Coordinator.Faulted => Faulted);
   function Uncertain return Domain_Set is
     (Domain_Set (Coordinator.Uncertain (Object)));
   procedure Acquire (Vendor, Device : Interfaces.Unsigned_16;
                      Fuse : Interfaces.Unsigned_32; Success : out Boolean) is
      Description : constant Inventory := Decode (Vendor, Device, Fuse);
   begin
      Success := False;
      if not Description.Valid then return; end if;
      Coordinator.Acquire (Object, Coordinator.Selection (Description.Domains), Success);
   end Acquire;
   procedure Release (Success : out Boolean) is
   begin
      Coordinator.Release (Object, Success);
   end Release;
end Intel_GPU_ADLN_Forcewake;
