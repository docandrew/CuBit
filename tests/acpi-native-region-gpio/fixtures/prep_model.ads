-- Hosted test model only; never a native runtime/provider implementation.
with Firmware_Tables; package Prep_Model is
 Connect_Calls,Disconnect_Calls,Prepare_Calls,Diagnostic_Calls,Physical_Calls:Natural:=0;
 Mode,Discoveries,Copies,Payloads,Closes,Raw_Allocations,Raw_Frees,Pauses,Resumes,Roots : Natural:=0;
 Prepare_OK,EC_Ready,Lock_Ready:Boolean:=True;
 Lose_Handler_After_Prefix,Fields_Pending:Boolean:=False;
 Initializer_Evaluations,Diagnostic_Open_Requests:Natural:=0;
 Pending,Busy,Uncertain : Boolean:=False;
 Micro_Now : Natural:=100;
 Raw : aliased Firmware_Tables.Bytes(1 .. 128) with Alignment=>16;
 Unavailable_Line_Seen,Unavailable_CMOS_Line,Unavailable_Path_Seen : Boolean := False;
 Unavailable_Lines : Natural := 0; Unavailable_Limited_Summary : Boolean := False;
end Prep_Model;