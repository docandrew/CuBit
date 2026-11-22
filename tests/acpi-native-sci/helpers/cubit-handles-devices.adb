with SCI_Probe;package body CuBit.Handles.Devices is
 use type Interfaces.Unsigned_16,Interfaces.Unsigned_64;
 function In_16(Ports:IO_Ports;Port:Interfaces.Unsigned_16) return Interfaces.Unsigned_64 is pragma Unreferenced(Ports);begin SCI_Probe.Reads:=SCI_Probe.Reads+1;return (if Port=16#400# then SCI_Probe.PM_Status elsif Port=16#402# then SCI_Probe.PM_Enable else 0);end;
 function In_8(Ports:IO_Ports;Port:Interfaces.Unsigned_16) return Interfaces.Unsigned_64 is begin return In_16(Ports,Port);end;
 function Out_16(Ports:IO_Ports;Port:Interfaces.Unsigned_16;Value:Interfaces.Unsigned_16) return Boolean is pragma Unreferenced(Ports);begin SCI_Probe.Writes:=SCI_Probe.Writes+1;if not SCI_Probe.Write_OK then return False;end if;if Port=16#400# then SCI_Probe.PM_Status:=SCI_Probe.PM_Status and not Interfaces.Unsigned_64(Value);elsif Port=16#402# then SCI_Probe.PM_Enable:=Interfaces.Unsigned_64(Value);end if;return True;end;
 function Out_8(Ports:IO_Ports;Port:Interfaces.Unsigned_16;Value:Interfaces.Unsigned_8) return Boolean is begin return Out_16(Ports,Port,Interfaces.Unsigned_16(Value));end;
end CuBit.Handles.Devices;