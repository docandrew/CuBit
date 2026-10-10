with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute;
with AML_Namespace;
with AML_Objects;
with AML_Table_Backing;
procedure Collecting_Reclamation_Tests is
   package N is new AML_Namespace (32, AML_Delays.Unavailable_Provider, Max_Retained_Roots => 8);
   package C is new N.Owned.Collecting;
   use type Byte;
   use type N.Load_Status;
   use type C.Access_Status;
   use type C.Collection_Count;
   use type AML_Execute.Execution_Status;
   A : C.Arena;
   Legacy : N.Owned.Arena;
   Input : aliased AML_Table_Backing.State (1,36);
   Args : C.Arguments := [others => (C.Immediate_Argument, 0)];
   Raw_Args : constant AML_Execute.Value_Arguments := [others => (AML_Execute.Integer_Datum,0,Ordinary_Integer)];
   Outcome : C.Result;
   Raw : AML_Execute.Execution_Result;
   Status : C.Access_Status;
   Loaded : N.Load_Status;
   Report : N.Initialization_Report;
   Pin : C.Value_Handle;
   Checks : Natural := 0;
   function Name (Text : String) return Bytes is
      B : Bytes (1 .. Text'Length);
   begin for I in B'Range loop B (I) := Character'Pos (Text (Text'First + (I - 1))); end loop; return B; end Name;
   function Method (Text : String; Count : Byte; Code : Bytes) return Bytes is
     (Bytes'[16#14#, Byte (Code'Length + 6)] & Name (Text) & Bytes'[Count] & Code);
   Buffer_Value : constant Bytes := [16#11#,4,16#0A#,1,16#55#];
   Fixture : constant Bytes :=
     Method ("TEMP",0, Bytes'[16#70#] & Buffer_Value & Bytes'[16#60#,16#A4#,1]) &
     Method ("BUFF",0, Bytes'[16#A4#] & Buffer_Value) &
     Method ("ECHO",1, Bytes'[16#70#] & Buffer_Value & Bytes'[16#60#,16#A4#,16#68#]) &
     Method ("OUTR",0, Bytes'[16#A4#] & Name ("PAIR") & Buffer_Value & Name ("BUFF")) &
     Method ("PAIR",2, Bytes'[16#70#] & Buffer_Value & Bytes'[16#60#,16#A4#,16#68#]);
   procedure Check (B : Boolean) is
   begin Checks := Checks + 1; if not B then raise Program_Error with Checks'Image; end if; end Check;
   procedure Check_Byte (H : C.Value_Handle) is
      Data : Bytes (1 .. 1); Copied : Natural;
   begin C.Read_Bytes (A,H,0,Data,Copied,Status); Check (Status=C.Available and Copied=1 and Data(1)=16#55#); end Check_Byte;
   procedure Drop (H : in out C.Value_Handle) is
   begin C.Release (A,H,Status); Check (Status=C.Available); end Drop;
   Iterations : constant := AML_Objects.Max_Objects / 2 + 3;
begin
   for Width in Integer_Width loop
      C.Reset (A,Status); Check(Status=C.Available);
      C.Load(A,Fixture,Width,Loaded,Status); Check(Status=C.Available and Loaded=N.Loaded);
      C.Seal(A,Report,Status); Check(Status=C.Available);
      C.Invoke(A,Input,2,Args,0,1000,Outcome,Status);
      Check(Status=C.Available and Outcome.Status=AML_Execute.Object_Returned); Pin:=Outcome.Handle;
      for I in 1 .. Iterations loop
         C.Invoke(A,Input,1,Args,0,1000,Outcome,Status);
         Check(Status=C.Available and Outcome.Status=AML_Execute.Returned);
      end loop;
      Check_Byte(Pin);
      Check(C.Reclamation_Metrics(A).Freed_Objects > C.Collection_Count(AML_Objects.Max_Objects));
      Check(C.Reclamation_Metrics(A).Freed_Bytes > C.Collection_Count(AML_Objects.Max_Objects));
      Check(C.Reclamation_Metrics(A).Rejected=0);
      Args(0):=(C.Retained_Argument,Pin);
      C.Invoke(A,Input,3,Args,1,1000,Outcome,Status);
      Check(Status=C.Available and Outcome.Status=AML_Execute.Object_Returned);
      declare H : C.Value_Handle:=Outcome.Handle; begin Check_Byte(H); Drop(H); end;
      Args:=[others=>(C.Immediate_Argument,0)];
      C.Invoke(A,Input,4,Args,0,1000,Outcome,Status);
      Check(Status=C.Available and Outcome.Status=AML_Execute.Object_Returned);
      declare H : C.Value_Handle:=Outcome.Handle; begin Check_Byte(H); Drop(H); end;
      Drop(Pin);
      declare OK : Boolean; Failed : Boolean:=False; begin
         N.Owned.Reset(Legacy,OK); Check(OK);
         N.Owned.Load(Legacy,Fixture,Width,Loaded); Check(Loaded=N.Loaded);
         for I in 1 .. Iterations loop
            N.Owned.Invoke(Legacy,Input,1,Raw_Args,0,1000,Raw);
            if Raw.Status=AML_Execute.Value_Limit then Failed:=True; exit; end if;
            Check(Raw.Status=AML_Execute.Returned);
         end loop;
         Check(Failed);
      end;
   end loop;
   Ada.Text_IO.Put_Line("COLLECTING RECLAMATION" & Checks'Image);
end Collecting_Reclamation_Tests;
