-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- Message-signaled device interrupts - implementation
-------------------------------------------------------------------------------
with acpi;
with Config;
with CPU_Topology;
with CPU_Topology.Boot;
with Device_Vectors;
with PerCPUData;
with Process.IPC;
with Process_Identities;
with Spinlocks;

package body Device_Interrupts with
    SPARK_Mode => Off
is
    use type Device_Vectors.Allocation_Status;
    use type Device_Vectors.Release_Status;
    use type Device_Vectors.Owner_Key;
    use type Device_Vectors.Vector_State;

    table : Device_Vectors.Vector_Table := Device_Vectors.EMPTY_TABLE;

    lock : Spinlocks.Spinlock;

    -- Each CPU counts only its own row, with interrupts off: no atomics.
    subtype Counting_CPU is Natural range 0 .. Config.MAX_SMP_CPUS - 1;
    type Vector_Counts is array (Vector) of Unsigned_64;
    counts : array (Counting_CPU) of Vector_Counts :=
      (others => (others => 0));
    strays : array (Counting_CPU) of Unsigned_64 := (others => 0);

    procedure enter is
    begin
        Spinlocks.enterCriticalSection (lock);
    end enter;

    procedure leave is
    begin
        Spinlocks.exitCriticalSection (lock);
    end leave;

    function keyOf (pid : Process.ProcessID) return Device_Vectors.Owner_Key is
      (Device_Vectors.Owner_Key
         (Process_Identities.To_Word (Process.identityOf (pid))));

    ---------------------------------------------------------------------------
    -- allocate
    ---------------------------------------------------------------------------
    procedure allocate (owner       : Process.ProcessID;
                        count       : Block_Size;
                        cpu         : Natural;
                        head        : out Vector;
                        destination : out Device_Vector_ABI.Destination_ID;
                        outcome     : out Allocation_Outcome)
    is
        status : Device_Vectors.Allocation_Status;
    begin
        head := Vector'First;
        destination := 0;
        if cpu >= acpi.numCPUs or else
           cpu >= CPU_Topology.Count (CPU_Topology.Boot.CPUs)
        then
            outcome := Invalid_CPU;
            return;
        end if;
        destination := Device_Vector_ABI.Destination_ID
          (CPU_Topology.Destination (CPU_Topology.Boot.CPUs, cpu));

        enter;
        Device_Vectors.Allocate (table, keyOf (owner), count, head, status);
        leave;

        outcome := (if status = Device_Vectors.Allocated then Allocated
                    else Vectors_Exhausted);
    end allocate;

    ---------------------------------------------------------------------------
    -- release
    ---------------------------------------------------------------------------
    procedure release (head     : Vector;
                       count    : out Block_Size;
                       owner    : out Unsigned_64;
                       released : out Boolean)
    is
        status : Device_Vectors.Release_Status;
    begin
        enter;
        count := table (head).Count;
        owner := Unsigned_64 (table (head).Owner);
        Device_Vectors.Release (table, head, status);
        leave;
        released := status = Device_Vectors.Released;
        if not released then
            count := 1;
            owner := 0;
        end if;
    end release;

    ---------------------------------------------------------------------------
    -- retireProcess
    ---------------------------------------------------------------------------
    procedure retireProcess (pid : Process.ProcessID) is
        key : constant Device_Vectors.Owner_Key := keyOf (pid);
    begin
        if key = Device_Vectors.NO_OWNER then
            return;
        end if;
        enter;
        Device_Vectors.Retire (table, key);
        leave;
    end retireProcess;

    ---------------------------------------------------------------------------
    -- deliver
    ---------------------------------------------------------------------------
    procedure deliver (v : Vector) is
        cpu : constant Natural := PerCPUData.getCPUNumber;
        key : constant Device_Vectors.Owner_Key :=
          Device_Vectors.Recipient (table, v);
        dest : Process.ProcessID := Process.NO_PROCESS;
    begin
        if key /= Device_Vectors.NO_OWNER then
            dest := Process.processOfIdentity
              (Process_Identities.From_Word (Unsigned_64 (key)));
        end if;
        if cpu in Counting_CPU then
            counts (cpu)(v) := counts (cpu)(v) + 1;
            if dest = Process.NO_PROCESS then
                strays (cpu) := strays (cpu) + 1;
            end if;
        end if;
        if dest /= Process.NO_PROCESS then
            Process.IPC.notifyDeviceVector (dest, v);
        end if;
    end deliver;

    ---------------------------------------------------------------------------
    -- Visibility
    ---------------------------------------------------------------------------
    function interruptCount (v : Vector) return Unsigned_64 is
        total : Unsigned_64 := 0;
    begin
        for c in Counting_CPU loop
            total := total + counts (c)(v);
        end loop;
        return total;
    end interruptCount;

    function ownerIdentity (v : Vector) return Unsigned_64 is
      (Unsigned_64 (table (v).Owner));

    function binding (v : Vector) return Device_Vector_ABI.Vector_Binding is
      (case table (v).State is
         when Device_Vectors.Free    => Device_Vector_ABI.Unbound,
         when Device_Vectors.Bound   => Device_Vector_ABI.Bound,
         when Device_Vectors.Retired => Device_Vector_ABI.Retired);

    function strayCount return Unsigned_64 is
        total : Unsigned_64 := 0;
    begin
        for c in Counting_CPU loop
            total := total + strays (c);
        end loop;
        return total;
    end strayCount;
end Device_Interrupts;
