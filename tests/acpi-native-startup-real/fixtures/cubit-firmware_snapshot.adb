pragma Ada_2022;
with CuBit.Handles.Testing;with Startup_Test_Firmware;with Owner_Test_Control;
package body CuBit.Firmware_Snapshot is
   package F renames Startup_Test_Firmware;
   package T renames Owner_Test_Control;
   use type CuBit.Handles.Firmware_Snapshot,T.Scenario;
   function Named(Label:String) return Authority is
      pragma Unreferenced(Label);
   begin return raise Program_Error with "fixture snapshot requires explicit held token";end Named;
   function Held(Handle:CuBit.Handles.Firmware_Snapshot) return Authority is
   begin return (Handle=>Handle);end Held;
   function Allowed(A:Authority) return Boolean is (A.Handle=CuBit.Handles.Testing.Snapshot);
   procedure Info(A:Authority;Result:out Catalog_Info;Status:out Outcome) is
      A_Size:constant Natural:=F.Image(F.DSDT_ID)'Length;
      B_Size:constant Natural:=F.Image(F.FADT_ID)'Length;
   begin
      T.Firmware_Infos:=T.Firmware_Infos+1;Result:=(others=><>);Status:=Not_Authorized;
      if not Allowed(A) or else T.Selected=T.Snapshot_Refused then return;end if;
      Result:=(Firmware_Snapshot_ABI.Interface_Version,Unsigned_32(F.Table_Count),True,
        Byte_Count(Natural'Max(A_Size,B_Size)),Byte_Count(A_Size+B_Size));Status:=Ok;
   end Info;
   procedure Describe(A:Authority;Table:Table_ID;Result:out Table_Description;Status:out Outcome) is
   begin
      T.Firmware_Descriptions:=T.Firmware_Descriptions+1;Result:=(others=><>);Status:=Not_Authorized;
      if not Allowed(A) then return;end if;
      if Table not in 1..Table_ID(F.Table_Count) then Status:=Unknown_Table;return;end if;
      declare Raw:constant F.Bytes:=F.Image(Positive(Table));begin
         for I in Signature_Index loop Result.Signature(I):=Unsigned_8(Raw(I));end loop;
         Result.Revision:=Unsigned_8(Raw(9));Result.Extent:=Byte_Count(Raw'Length);Status:=Ok;
      end;
   end Describe;
   procedure Copy_Part(A:Authority;Table:Table_ID;Offset:Byte_Count;
      Into:out Byte_Array;Copied:out Byte_Count;Status:out Outcome) is
   begin
      T.Firmware_Copies:=T.Firmware_Copies+1;Into:=[others=>0];Copied:=0;Status:=Not_Authorized;
      if not Allowed(A) then return;end if;
      if Table not in 1..Table_ID(F.Table_Count) then Status:=Unknown_Table;return;end if;
      declare Raw:constant F.Bytes:=F.Image(Positive(Table));begin
         if Offset>Byte_Count(Raw'Length) then Status:=Beyond_End;return;end if;
         if T.Selected=T.Copy_Refused then Status:=Copy_Failed;return;end if;
         if Into'Length=0 and then Offset<Byte_Count(Raw'Length) then Status:=Empty_Before_End;return;end if;
         Copied:=Byte_Count(Natural'Min(Into'Length,Raw'Length-Natural(Offset)));
         for I in 1..Natural(Copied) loop Into(Into'First+I-1):=Unsigned_8(Raw(Natural(Offset)+I));end loop;
         Status:=Ok;
      end;
   end Copy_Part;
end CuBit.Firmware_Snapshot;
