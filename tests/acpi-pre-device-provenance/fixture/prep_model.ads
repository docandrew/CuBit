-- Hosted test model only; never a native runtime/provider implementation.
with Firmware_Tables;with Interfaces; package Prep_Model is
 Source_Device:Interfaces.Unsigned_64:=16#F987_6543_0000_0009#;
 Mode,Discoveries,Copies,Payloads,Closes,Raw_Allocations,Raw_Frees,Pauses,Resumes,Roots : Natural:=0;
 Pending,Busy,Uncertain : Boolean:=False;
 Micro_Now : Natural:=100;
 Raw : aliased Firmware_Tables.Bytes(1 .. 32) with Alignment=>16;
end Prep_Model;