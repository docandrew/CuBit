-- Hosted test model only; never a native runtime/provider implementation.
with Firmware_Tables; package Prep_Model is
 Mode,Discoveries,Copies,Payloads,Closes,Raw_Allocations,Raw_Frees,Pauses,Resumes,Roots : Natural:=0;
 Pending,Busy,Uncertain : Boolean:=False;
 Micro_Now : Natural:=100;
 Raw : aliased Firmware_Tables.Bytes(1 .. 128) with Alignment=>16;
end Prep_Model;