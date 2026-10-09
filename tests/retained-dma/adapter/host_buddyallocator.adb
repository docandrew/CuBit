with Ada.Dispatching;
with Spinlocks;
package body BuddyAllocator is
   use Interfaces;
   use System;
   use System.Storage_Elements;
   type Page_Data is array (1 .. 512) of Unsigned_64;
   RAM : array (1 .. 1024) of aliased Page_Data with Alignment => 4096;
   Occupied : array (RAM'Range) of Boolean := [others => False];
   Charges : array (RAM'Range) of Unsigned_64 := [others => 0];
   Refund : Charge_Refund_Handler := null;
   protected State is
      procedure Take (Page : out Address);
      procedure Put (Page : Address; Charge : out Unsigned_64);
      procedure Bind (Page : Address; Charge : Unsigned_64; OK : out Boolean);
      procedure Fail_Bind (Enabled : Boolean);
      procedure Fail (Nth : Natural);
      function Bytes return Unsigned_64;
   private
      Fail_At, Attempts, Used : Natural := 0;
      Bind_Fails : Boolean := False;
   end State;
   protected body State is
      procedure Take (Page : out Address) is
      begin
         Page := Null_Address;
         Attempts := Attempts + 1;
         if Attempts = Fail_At then return; end if;
         for I in RAM'Range loop
            if not Occupied (I) then
               Occupied (I) := True;
               Used := Used + 1;
               Page := RAM (I)'Address;
               return;
            end if;
         end loop;
      end Take;
      procedure Put (Page : Address; Charge : out Unsigned_64) is
         I : constant Natural := Natural
           ((To_Integer (Page) - To_Integer (RAM'Address)) / 4096) + 1;
      begin
         pragma Assert (I in RAM'Range and then Occupied (I));
         pragma Assert (Page = RAM (I)'Address);
         Occupied (I) := False;
         Charge := Charges (I);
         Charges (I) := 0;
         Used := Used - 1;
      end Put;
      procedure Bind (Page : Address; Charge : Unsigned_64; OK : out Boolean) is
         I : constant Natural := Natural
           ((To_Integer (Page) - To_Integer (RAM'Address)) / 4096) + 1;
      begin
         pragma Assert (I in RAM'Range and then Occupied (I));
         pragma Assert (Page = RAM (I)'Address);
         OK := not Bind_Fails and then Charge /= 0 and then Charges (I) = 0;
         if OK then Charges (I) := Charge; end if;
      end Bind;
      procedure Fail_Bind (Enabled : Boolean) is
      begin
         Bind_Fails := Enabled;
      end Fail_Bind;
      procedure Fail (Nth : Natural) is
      begin
         Fail_At := Nth;
         Attempts := 0;
      end Fail;
      function Bytes return Unsigned_64 is (Unsigned_64 (Used) * 4096);
   end State;
   procedure installChargeRefundHandler
     (Handler : not null Charge_Refund_Handler; Success : out Boolean) is
   begin
      Success := Refund = null;
      if Success then Refund := Handler; end if;
   end installChargeRefundHandler;
   function getTotalBytes return Storage_Count is (512 * 1024 * 1024);
   procedure alloc (Order : Natural; Page : out Address) is
   begin
      pragma Assert (not Spinlocks.Held_By_Caller);
      pragma Assert (Order = 0);
      Ada.Dispatching.Yield;
      State.Take (Page);
      Ada.Dispatching.Yield;
   end alloc;
   procedure free (Order : Natural; Page : Address) is
      Charge : Unsigned_64;
   begin
      pragma Assert (not Spinlocks.Held_By_Caller);
      pragma Assert (Order = 0);
      Ada.Dispatching.Yield;
      State.Put (Page, Charge);
      -- Actual mock physical return precedes callback, outside the lock.
      if Charge /= 0 then Refund (Charge, 1); end if;
      Ada.Dispatching.Yield;
   end free;
   procedure allocFrame (Frame : out Virtmem.PhysAddress) is
      Page : Address;
   begin
      alloc (0, Page);
      Frame := To_Integer (Page);
   end allocFrame;
   procedure freeFrame (Frame : Virtmem.PhysAddress) is
   begin
      free (0, To_Address (Frame));
   end freeFrame;
   procedure bindKernelFrameCharge
     (Frame : Virtmem.PhysAddress; Charge : Unsigned_64; OK : out Boolean) is
   begin
      pragma Assert (not Spinlocks.Held_By_Caller);
      State.Bind (To_Address (Frame), Charge, OK);
   end bindKernelFrameCharge;
   procedure Set_Bind_Failure (Enabled : Boolean) is
   begin
      State.Fail_Bind (Enabled);
   end Set_Bind_Failure;
   procedure Set_Failure (Nth : Natural) is
   begin
      State.Fail (Nth);
   end Set_Failure;
   function Live_Bytes return Unsigned_64 is (State.Bytes);
   procedure Complete_Physical (Charge, Pages : Unsigned_64) is
   begin
      Refund (Charge, Pages);
   end Complete_Physical;
end BuddyAllocator;
