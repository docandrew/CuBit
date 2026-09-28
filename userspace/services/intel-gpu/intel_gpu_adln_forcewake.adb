with Intel_GPU_Forcewake;
with Intel_GPU_Domain_Lease;
package body Intel_GPU_ADLN_Forcewake is
   use Intel_GPU_ADLN_Inventory;
   generic
      Item : Domain;
   package Binding is
      procedure Acquire (Success : out Boolean);
      procedure Release (Success : out Boolean);
   end Binding;
   package body Binding is
      package FW is new Intel_GPU_Forcewake
        (Read_32, Write_32, Pause, Now_Milliseconds,
         Request_Register (Item), Ack_Register (Item));
      Object : FW.Lease;
      use type FW.Result;
      procedure Acquire (Success : out Boolean) is
         Status : FW.Result;
      begin
         FW.Acquire (Object, 100, Status);
         Success := Status = FW.Ready;
      end Acquire;
      procedure Release (Success : out Boolean) is
         Status : FW.Result;
      begin
         FW.Release (Object, 100, Status);
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
