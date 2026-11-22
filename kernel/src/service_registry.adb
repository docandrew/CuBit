-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- The service registry (service_registry.ads).
-------------------------------------------------------------------------------
with Process_Identities;
with Publication_Pages;
with TextIO; use TextIO;

package body Service_Registry is

    type DriverList is array (DriverID) of Process.ProcessID;
    registeredDrivers : DriverList := (others => Process.NO_PROCESS)
        with Volatile;

    function Registered (Driver : DriverID) return Process.ProcessID is
      (registeredDrivers (Driver))
        with SPARK_Mode => Off;

    procedure Register (pid : Process.ProcessID; driver : DriverID)
        with SPARK_Mode => Off
    is
    begin
        registeredDrivers (driver) := pid;
        Publication_Pages.Set_Registered
          (driver, Process_Identities.To_Word (Process.identityOf (pid)));
        print ("registry: registered drv ");
        printd (Unsigned_64 (driver));
        print (" = pid ");
        printd (Unsigned_64 (pid));
        println;
    end Register;

    procedure Unregister_Process (pid : Process.ProcessID)
        with SPARK_Mode => Off
    is
    begin
        for d in DriverID loop
            if registeredDrivers (d) = pid then
                registeredDrivers (d) := Process.NO_PROCESS;
                Publication_Pages.Set_Registered (d, System_Publications.No_Identity);
            end if;
        end loop;
    end Unregister_Process;

end Service_Registry;
