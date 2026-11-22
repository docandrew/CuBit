with Intel_GPU_ADLN_Inventory;
with Intel_GPU_Engine_Stop;
with Intel_GPU_Reset_Prepare;
with Intel_GPU_GT_Reset;
with Intel_GPU_Handoff;
package body Intel_GPU_ADLN_Reset is
   use Interfaces;
   use Intel_GPU_ADLN_Inventory;
   Failed_Engine : Natural := 0;
   function Failure_Engine return Natural is (Failed_Engine);
   procedure Stop (Item : Engine; Success : out Boolean) is
      Base : constant Unsigned_32 := Engine_Base (Item);
      function Mode return Unsigned_32 is (Read (Base + 16#9C#));
      function Pending return Unsigned_32 is (Read (Pending_Register (Item)));
      function Power return Unsigned_32 is (Read (16#A2A0#));
      procedure Mode_Write (Value : Unsigned_32) is
      begin Write (Base + 16#9C#, Value); end;
      procedure Prefetch_Write (Value : Unsigned_32) is
      begin Write (Base + 16#29C#, Value); end;
      package Stopper is new Intel_GPU_Engine_Stop
        (Mode, Pending, Power, Mode_Write, Prefetch_Write, Now, Pause);
      Status : Stopper.Result;
      use type Stopper.Result;
   begin
      Stopper.Stop (Poll_Limit, Status);
      Success := Status = Stopper.Stopped;
      if not Success then Failed_Engine := Engine'Pos (Item) + 1; end if;
   end Stop;
   procedure Prepare (Item : Engine; Success : out Boolean) is
      Offset : constant Unsigned_32 := Engine_Base (Item) + 16#D0#;
      function Control return Unsigned_32 is (Read (Offset));
      procedure Control_Write (Value : Unsigned_32) is
      begin Write (Offset, Value); end;
      package Prep is new Intel_GPU_Reset_Prepare (Control, Control_Write, Pause, Now);
      Status : Prep.Result;
      use type Prep.Result;
   begin
      Prep.Prepare (Poll_Limit, Status);
      Success := Status = Prep.Ready;
      if not Success then Failed_Engine := Engine'Pos (Item) + 1; end if;
   end Prepare;
   procedure Reset (Success : out Boolean) is
      function Control return Unsigned_32 is (Read (16#941C#));
      procedure Control_Write (Value : Unsigned_32) is
      begin Write (16#941C#, Value); end;
      package GT is new Intel_GPU_GT_Reset (Control, Control_Write, Now, Pause);
      Object : GT.Attempt;
      Status : GT.Result;
      use type GT.Result;
   begin
      GT.Execute (Object, Poll_Limit, Status);
      Success := Status = GT.Complete;
   end Reset;
   procedure Cancel (Item : Engine; Success : out Boolean) is
      Offset : constant Unsigned_32 := Engine_Base (Item) + 16#D0#;
      Value : Unsigned_32;
   begin
      Write (Offset, 16#10000#);
      Value := Read (Offset);
      Success := Value /= Unsigned_32'Last and then (Value and 1) = 0;
      if not Success then Failed_Engine := Engine'Pos (Item) + 1; end if;
   end Cancel;
   package Handoff is new Intel_GPU_Handoff (Hold, Stop, Prepare, Reset, Cancel);
   Object : Handoff.Attempt;
   procedure Execute (Vendor, Device : Unsigned_16; Fuse : Unsigned_32;
                      Status : out Result) is
      Value : Handoff.Result;
   begin
      Handoff.Execute (Object, Vendor, Device, Fuse, Value);
      Status := (case Value is
         when Handoff.Rejected => Rejected,
         when Handoff.Forcewake_Failed => Forcewake_Failed,
         when Handoff.Stop_Failed => Stop_Failed,
         when Handoff.Prepare_Failed => Prepare_Failed,
         when Handoff.Reset_Failed => Reset_Failed,
         when Handoff.Cleanup_Failed => Cleanup_Failed,
         when Handoff.Complete => Complete);
   end Execute;
end Intel_GPU_ADLN_Reset;
