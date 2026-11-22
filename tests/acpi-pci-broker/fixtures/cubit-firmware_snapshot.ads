pragma Ada_2022;
with Interfaces;use Interfaces;with CuBit.Handles;with Firmware_Snapshot_ABI;
package CuBit.Firmware_Snapshot is
 type Authority is null record;type Table_ID is new Unsigned_32;type Byte_Count is new Unsigned_64;
 Chunk_Max:constant:=Firmware_Snapshot_ABI.Chunk_Max;
 type Outcome is(Ok,Not_Ready,Unknown_Table,Beyond_End,Empty_Before_End,Copy_Failed,Not_Authorized,Malformed);
 type Catalog_Info is record Table_Count:Unsigned_32:=1;Ready:Boolean:=True;end record;
 subtype Signature_Index is Positive range 1..4;type Signature_Bytes is array(Signature_Index)of Unsigned_8;
 type Table_Description is record Signature:Signature_Bytes:=[77,67,70,71];Extent:Byte_Count:=0;end record;
 type Byte_Array is array(Positive range <>)of Unsigned_8;
 Data : Byte_Array(1..8192):=[others=>0];Length:Natural:=0;
 Copies,Describe_Calls:Natural:=0;Fail_Copy,Short_Copy,Duplicate,Fail_Describe,Not_Ready_Now,Fail_After_First,Missing,Fail_Info:Boolean:=False;
 function Held(Handle:CuBit.Handles.Firmware_Snapshot)return Authority;
 procedure Info(A:Authority;Result:out Catalog_Info;Status:out Outcome);
 procedure Describe(A:Authority;Table:Table_ID;Result:out Table_Description;Status:out Outcome);
 procedure Copy_Part(A:Authority;Table:Table_ID;Offset:Byte_Count;Into:out Byte_Array;Copied:out Byte_Count;Status:out Outcome);
end;
