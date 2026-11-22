pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages;
with CuBit.Grant_References;
with Firmware_Tables;
package Bridge_Trace is
 type Scenario is (Normal, Lost_Start, Lost_Import, Lost_Finish, Pending_Return,
                   Bad_Table, Wrong_Stamp, Create_False);
 Mode : Scenario := Normal;
 type Notify_Fault is (No_Notify_Fault, Lose_Query, Lose_Interest,
   Malform_Interest, Malform_Finish, Deny_Interest, Lose_Claim, Lose_Ack, Malform_Receipt);
 Fault : Notify_Fault := No_Notify_Fault;
 Interest_Calls, Claim_Calls, Ack_Calls : Natural := 0;
 Last_Ack : CuBit.Messages.MessageWords := [others=>0];
 type Event is (Allocation, Grant_Create, IPC_Call, Grant_Acquire,
                Grant_Return, Grant_Revoke, Retirement_Query);
 type Events is array(Positive range 1..256) of Event;
 Log : Events := [others=>Allocation]; Used : Natural := 0;
 procedure Note(E : Event);
 Buffer : aliased Firmware_Tables.Bytes(1..8192) := [others=>16#CC#];
 for Buffer'Alignment use 4096;
 Allocated_Bytes : Unsigned_64 := 0;
 Grant_Pages : Natural := 0;
 Grant_Image : Firmware_Tables.Bytes(1..8192) := [others=>0];
 Now : Unsigned_64 := 100;
 Created_OK : Boolean := True;
 Active, Acquired, Revoked, Allow_Return : Boolean := False;
 Reference : CuBit.Grant_References.Reference;
 Last_Message : CuBit.Messages.Message;
 Last_Deadline : Unsigned_64 := 0;
 Creates, Calls, Revokes, Queries, Acquires, Returns : Natural := 0;
 Import_Calls, Finish_Calls, Start_Calls : Natural := 0;
end Bridge_Trace;
