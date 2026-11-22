pragma Ada_2022;
package body CuBit.Firmware_Snapshot is
 function Held(Handle:CuBit.Handles.Firmware_Snapshot)return Authority is pragma Unreferenced(Handle);begin return(null record);end;
 procedure Info(A:Authority;Result:out Catalog_Info;Status:out Outcome)is pragma Unreferenced(A);begin Result:=(Table_Count=>(if Duplicate then 2 else 1),Ready=>not Not_Ready_Now);Status:=(if Fail_Info then Copy_Failed else Ok);end;
 procedure Describe(A:Authority;Table:Table_ID;Result:out Table_Description;Status:out Outcome)is pragma Unreferenced(A,Table);begin Describe_Calls:=Describe_Calls+1;Result:=(Extent=>Byte_Count(Length),others=><>);if Missing then Result.Signature:=[70,65,67,80];end if;Status:=(if Fail_Describe then Copy_Failed else Ok);end;
 procedure Copy_Part(A:Authority;Table:Table_ID;Offset:Byte_Count;Into:out Byte_Array;Copied:out Byte_Count;Status:out Outcome)is pragma Unreferenced(A,Table);begin Copies:=Copies+1;Into:=[others=>0];if Fail_Copy or (Fail_After_First and Copies>1) then Copied:=0;Status:=Copy_Failed;return;end if;
 Copied:=Byte_Count(Into'Length)-(if Short_Copy then 1 else 0);Status:=Ok;
 for K in 1..Natural(Copied)loop Into(Into'First+K-1):=Data(Natural(Offset)+K);end loop;end;
end;
