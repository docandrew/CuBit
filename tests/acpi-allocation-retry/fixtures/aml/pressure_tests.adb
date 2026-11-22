with Ada.Text_IO;
with AML_Delays;
with AML_Decode; use AML_Decode;
with AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
procedure Pressure_Tests is
   package N is new AML_Namespace
     (32, AML_Delays.Unavailable_Provider, Max_Retained_Roots => 16);
   package C is new N.Owned.Collecting;
   use type N.Load_Status;
   use type C.Access_Status;
   use type C.Collection_Count;
   use type AML_Execute.Execution_Status;
   use type Byte;
   use type Integer_Value;
   use type Bytes;
   Object_Slots : constant := 8;
   A : C.Arena (Object_Slots,64,16);
   Input : aliased AML_Table_Backing.State (1,36);
   Args : constant C.Arguments := [others => (C.Immediate_Argument,0)];
   Outcome : C.Result;
   Status : C.Access_Status;
   Loaded : N.Load_Status;
   Report : N.Initialization_Report;
   Pins : array (1 .. Object_Slots) of C.Value_Handle;
   Before : C.Collection_Statistics;
   Checks : Natural := 0;
   function Name (Text : String) return Bytes is
      B : Bytes (1 .. Text'Length);
   begin
      for I in B'Range loop B(I):=Character'Pos(Text(Text'First+I-1)); end loop;
      return B;
   end Name;
   function Method (Text : String; Code : Bytes) return Bytes is
     (Bytes'[16#14#,Byte(Code'Length+6)] & Name(Text) & Bytes'[0] & Code);
   Buffer_Value : constant Bytes := [16#11#,4,16#0A#,1,16#55#];
   Fixture : constant Bytes :=
     Method("TEMP",Bytes'[16#70#] & Buffer_Value & Bytes'[16#60#,16#A4#,1]) &
     Method("BUFF",Bytes'[16#A4#] & Buffer_Value);
   procedure Check (B : Boolean; Label_Text : String) is
   begin
      Checks:=Checks+1;
      if not B then raise Program_Error with Label_Text & Checks'Image; end if;
   end Check;
   procedure Read_Pin (H : C.Value_Handle) is
      Data : Bytes (1..1); Copied : Natural;
   begin
      C.Read_Bytes(A,H,0,Data,Copied,Status);
      Check(Status=C.Available and Copied=1 and Data(1)=16#55#,"retained byte");
   end Read_Pin;
   procedure Invoke (Which : Positive) is
   begin C.Invoke(A,Input,Which,Args,0,1000,Outcome,Status); end Invoke;
   procedure Check_Quota (Buffer_Case : Boolean; Width : Integer_Width) is
      Q : C.Arena (32,(if Buffer_Case then 2 else 64),(if Buffer_Case then 16 else 2));
      Q_Pins : array (1..2) of C.Value_Handle;
      Child : C.Value_Handle;
      Q_Args : constant C.Arguments := [others => (C.Immediate_Argument,0)];
      Q_Result : C.Result;
      Q_Status : C.Access_Status;
      Q_Loaded : N.Load_Status;
      Q_Report : N.Initialization_Report;
      Q_Before : C.Collection_Statistics;
      Package_Value : constant Bytes := [16#12#,4,1,16#0A#,16#55#];
      Q_Fixture : constant Bytes := Method("QUOT",Bytes'[16#A4#] &
         (if Buffer_Case then Buffer_Value else Package_Value));
      procedure Call is
      begin C.Invoke(Q,Input,1,Q_Args,0,1000,Q_Result,Q_Status); end Call;
      procedure Inspect is
         D : C.Value_Description;
      begin
         for H of Q_Pins loop
            C.Describe(Q,H,D,Q_Status); Check(Q_Status=C.Available,"quota root survives");
            if not Buffer_Case then
               C.Read_Element(Q,H,0,Child,Q_Status);
               Check(Q_Status=C.Available,"package child survives");
               C.Describe(Q,Child,D,Q_Status);
               Check(Q_Status=C.Available and D.Number=16#55#,"package value survives");
               C.Release(Q,Child,Q_Status); Check(Q_Status=C.Available,"release child");
            end if;
         end loop;
      end Inspect;
   begin
      C.Reset(Q,Q_Status); Check(Q_Status=C.Available,"quota reset");
      C.Load(Q,Q_Fixture,Width,Q_Loaded,Q_Status);
      Check(Q_Status=C.Available and Q_Loaded=N.Loaded,"quota load");
      C.Seal(Q,Q_Report,Q_Status); Check(Q_Status=C.Available,"quota seal");
      Q_Before:=C.Reclamation_Metrics(Q);
      for H of Q_Pins loop
         Call; Check(Q_Status=C.Available and Q_Result.Status=AML_Execute.Object_Returned,"quota fits");
         H:=Q_Result.Handle;
      end loop;
      Check(C.Reclamation_Metrics(Q).Attempted=Q_Before.Attempted,"quota fits no GC");
      Q_Before:=C.Reclamation_Metrics(Q);
      Call;
      Check(Q_Status=C.Available and Q_Result.Status=AML_Execute.Value_Limit,"quota capacity failure");
      Check(C.Reclamation_Metrics(Q).Attempted=Q_Before.Attempted+1,"quota failure one GC");
      Inspect;
      C.Release(Q,Q_Pins(1),Q_Status); Check(Q_Status=C.Available,"quota unpin");
      Q_Before:=C.Reclamation_Metrics(Q);
      Call;
      Check(Q_Status=C.Available and Q_Result.Status=AML_Execute.Object_Returned,"quota retry succeeds");
      Q_Pins(1):=Q_Result.Handle;
      Check(C.Reclamation_Metrics(Q).Attempted=Q_Before.Attempted+1,"quota recovery one GC");
      Inspect;
      for H of Q_Pins loop C.Release(Q,H,Q_Status); Check(Q_Status=C.Available,"quota cleanup"); end loop;
   end Check_Quota;

begin
   for Width in Integer_Width loop
      Check_Quota(True,Width); Check_Quota(False,Width);
      C.Reset(A,Status); Check(Status=C.Available,"reset");
      C.Load(A,Fixture,Width,Loaded,Status);
      Check(Status=C.Available and Loaded=N.Loaded,"load");
      C.Seal(A,Report,Status); Check(Status=C.Available,"seal");
      Before:=C.Reclamation_Metrics(A);
      Invoke(2); Check(Status=C.Available and Outcome.Status=AML_Execute.Object_Returned,"first buffer");
      Pins(1):=Outcome.Handle;
      Check(C.Reclamation_Metrics(A).Attempted=Before.Attempted,"fits without collection");
      for I in 1..100 loop
         Invoke(1); Check(Status=C.Available and Outcome.Status=AML_Execute.Returned,"temporary allocation");
      end loop;
      Read_Pin(Pins(1));
      Check(C.Reclamation_Metrics(A).Completed>Before.Completed,"real reclamation occurred");
      Check(C.Reclamation_Metrics(A).Freed_Objects>Before.Freed_Objects,"garbage freed");
      Check(C.Reclamation_Metrics(A).Rejected=Before.Rejected,"valid roots");
      for I in 2..Object_Slots loop
         Invoke(2);
         Check(Status=C.Available and Outcome.Status=AML_Execute.Object_Returned,"fill retained arena");
         Pins(I):=Outcome.Handle;
      end loop;
      Before:=C.Reclamation_Metrics(A);
      Invoke(2);
      Check(Status=C.Available and Outcome.Status=AML_Execute.Value_Limit,"full live store refuses");
      Check(C.Reclamation_Metrics(A).Attempted=Before.Attempted+1,"one collection before final refusal");
      Check(C.Reclamation_Metrics(A).Completed=Before.Completed+1,"one completed collection");
      Check(C.Reclamation_Metrics(A).Freed_Objects=Before.Freed_Objects,"all objects live");
      for H of Pins loop Read_Pin(H); end loop;
      C.Release(A,Pins(1),Status); Check(Status=C.Available,"release live root");
      Before:=C.Reclamation_Metrics(A);
      Invoke(2);
      Check(Status=C.Available and Outcome.Status=AML_Execute.Object_Returned,"retry after reclaim succeeds");
      Pins(1):=Outcome.Handle;
      Check(C.Reclamation_Metrics(A).Attempted=Before.Attempted+1,"one collection for successful retry");
      for H of Pins loop Read_Pin(H); end loop;
      for H of Pins loop C.Release(A,H,Status); Check(Status=C.Available,"cleanup"); end loop;
   end loop;
   Ada.Text_IO.Put_Line("REAL COLLECTING PRESSURE" & Checks'Image);
end Pressure_Tests;
