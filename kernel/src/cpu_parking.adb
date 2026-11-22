pragma Ada_2022;
with System;
with System.Machine_Code; use System.Machine_Code;
with Interfaces; use Interfaces;
with IPI;

package body CPU_Parking is
    No_Owner : constant Unsigned_64 := 0;

    -- The parking CPU plus one, or No_Owner.
    owner : aliased Unsigned_64 := No_Owner with Volatile;
    requestedFlag : Boolean := False with Atomic;
    type Parked_Flags is array (CPU_Index) of Boolean with Atomic_Components;
    parked : Parked_Flags := [others => False];
    parkedSeen : Natural := 0 with Volatile;

    function compareAndSwap (Ptr : System.Address; Expected, Desired : Unsigned_64)
      return Unsigned_64
      with Import, Convention => Intrinsic,
           External_Name => "__sync_val_compare_and_swap_8";

    procedure pause is
    begin
        Asm ("pause", Volatile => True);
    end pause;

    function Requested return Boolean is (requestedFlag);

    procedure Hold (Self : CPU_Index) is
    begin
        if not requestedFlag or else owner = Unsigned_64 (Self) + 1 then
            return;
        end if;
        parked (Self) := True;
        while requestedFlag loop
            pause;
        end loop;
        parked (Self) := False;
    end Hold;

    procedure Park_Others (Self : CPU_Index; Online : CPU_Set; Result : out Outcome) is
        function All_Stopped return Boolean is
          (for all C in CPU_Index => C = Self or else not Online (C) or else parked (C));
        Count : Natural := 0;
    begin
        if compareAndSwap (owner'Address, No_Owner, Unsigned_64 (Self) + 1) /= No_Owner then
            Result := Busy;
            return;
        end if;
        requestedFlag := True;
        IPI.broadcastReschedule;
        Result := Some_Running;
        for Poll in 1 .. Wait_Iterations loop
            if All_Stopped then
                Result := All_Parked;
                exit;
            end if;
            pause;
        end loop;
        for C in CPU_Index loop
            if C /= Self and then parked (C) then
                Count := Count + 1;
            end if;
        end loop;
        parkedSeen := Count;
    end Park_Others;

    procedure Release is
    begin
        requestedFlag := False;
        owner := No_Owner;
    end Release;

    function Parked_Count return Natural is (parkedSeen);
end CPU_Parking;
