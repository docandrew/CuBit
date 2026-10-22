with Ada.Text_IO;
with AML_Delays;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Names;
with AML_Objects;
with AML_Objects.Reclamation;
with AML_Identity;
with AML_Identity.Issuer;
with AML_References;
with AML_Table_Backing;
procedure Field_Owner_Tests is
   use type AML_Decode.Bytes;
   procedure Clock (Value : out Integer_Value; Available : out Boolean);
   package N is new AML_Namespace (24, AML_Delays.Unavailable_Provider, Clock);
   use N; use N.Owned;
   use type Integer_Value;
   use type Byte;
   use type AML_Objects.Allocation_Status;
   use type AML_Objects.Object_Kind;
   use type AML_References.Reference;
   use type AML_References.Node_Position;
   use type AML_Objects.Reclamation.Reclaim_Status;
   A, Foreign : Arena;
   Input : aliased AML_Table_Backing.State (1, 1);
   Width : Integer_Width := Bits_64;
   Status : Execution_Status := Returned;
   Checks : Natural := 0;
   Running : Boolean := False;
   Reuse_Only : Boolean := False;
   Last_Field : Reference;
   Keep : Object_Root_Set;
   Workspace : Root_Workspace;
   Trace : Root_Trace_Status;
   Source : Datum;
   Backing : AML_References.Object_Handle;
   Raw : constant Bytes := [16#81#,16#42#,16#A5#,16#5A#,1,2,3,4,5,6];
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Checks'Image & Status'Image; end if;
   end Check;
   function Path (S : String) return AML_Names.Name_Result is
      B : Bytes (1 .. S'Length);
   begin
      for I in B'Range loop B (I) := Byte (Character'Pos (S (S'First + (I - 1)))); end loop;
      return AML_Names.Read_Name (B);
   end Path;
   procedure Verify (V : Datum; Expected : Bytes) is
   begin
      Check (V.Value_Kind = Object_Datum);
      Check (AML_Objects.Kind (Value_Store (Snapshot (A)), V.Object.ID) = AML_Objects.Buffer_Object);
      Check (AML_Objects.Byte_Data (Value_Store (Snapshot (A)), V.Object.ID) = Expected);
   end Verify;
   procedure Clock (Value : out Integer_Value; Available : out Boolean) is
      Fields : array (1 .. 8) of Reference;
      Sizes : constant array (1 .. 8) of Integer_Value := [1,8,16,32,64,1,9,72];
      V : Datum;
      Ref : Reference;
      Before : State;
      OK : Boolean;
      ID : AML_Objects.Object_ID;
      Alloc : AML_Objects.Allocation_Status;
      Root : Retained_Root;
      Pinned : Retain_Status;
      Released_Status : Release_Status;
      Pkg, Bad : Datum;
      Pkg_Handle : AML_References.Object_Handle;
      Pkg_Ref : Reference;
      procedure Verify_First_Bit (Bit : Integer_Value; First_Byte : Byte) is
         Readback : Datum;
         Expected : Bytes := Raw;
      begin
         Expected (Expected'First) := First_Byte;
         Read_Buffer_Field (A, Fields (1), Width, Readback, Status);
         Check (Status = Returned and then Readback = (Integer_Datum, Bit, Ordinary_Integer));
         Check (Kind (Snapshot (A), Node_ID (AML_References.Named_Node (Fields (1)))) = Buffer_Field_Object);
         Check (Matches (A, Fields (1)));
         Check (AML_Objects.Byte_Data (Value_Store (Snapshot (A)), Source.Object.ID) = Expected);
      end Verify_First_Bit;
   begin
      Value := 0; Available := True;
      if not Running then return; end if;
      if Reuse_Only then
         for I in 1 .. 5 loop
            Define_Buffer_Field(A,2,Path("REU" & Character'Val(48+I)),Backing,0,1,False,Ref,Status);
            Check(Status=Returned);
         end loop;
         Check(AML_References.Named_Node(Ref)=AML_References.Named_Node(Last_Field));
         Read_Buffer_Field(A,Last_Field,Width,V,Status);Check(Status=Unsupported_Value);
         Read_Buffer_Field(A,Ref,Width,V,Status);Check(Status=Returned);
         return;
      end if;
      for I in Fields'Range loop
         declare Name : constant String := "FLD" & Character'Val (Character'Pos ('0') + I); begin
            Define_Buffer_Field (A, 2, Path (Name), Backing, 0, Sizes (I), I >= 6, Fields (I), Status);
            Check (Status = Returned);
            Read_Buffer_Field (A, Fields (I), Width, V, Status); Check (Status = Returned);
            if I >= 6 or else (I = 5 and Width = Bits_32) then
               declare Expected : Bytes (1 .. Natural ((Sizes (I) + 7) / 8)); begin
                  Expected := Raw (1 .. Expected'Length);
                  if Sizes (I) mod 8 /= 0 then
                     Expected (Expected'Last) := Expected (Expected'Last) mod 2 ** Natural (Sizes (I) mod 8);
                  end if;
                  Verify (V, Expected);
               end;
            else
               declare Expected : Integer_Value := 0; begin
                  for K in 0 .. Natural (Sizes (I)) - 1 loop
                     if (Raw (1 + K / 8) / 2 ** (K mod 8)) mod 2 = 1 then Expected := Expected + 2 ** K; end if;
                  end loop;
                  Check (V.Value_Kind = Integer_Datum and then V.Number = Expected);
               end;
            end if;
         end;
      end loop;
      Last_Field := Fields (5);
      -- All target policies write through an existing field and preserve its
      -- metadata/identity; neighbor bits and later bytes must remain unchanged.
      for Mode in Reference_Store_Mode loop
         Store_Reference_Value (A, Fields (1), Width, (Integer_Datum, 0, Ordinary_Integer), Status, Mode);
         Check (Status = Returned); Verify_First_Bit (0, 16#80#);
         Store_Reference_Value (A, Fields (1), Width, (Integer_Datum, 5, Ordinary_Integer), Status, Mode);
         Check (Status = Returned); Verify_First_Bit (1, 16#81#);
      end loop;
      Copy_And_Attach (A, (Referenced_Destination, Fields (1)), Width,
        (Integer_Datum, 0, Ordinary_Integer), V, Status);
      Check (Status = Returned); Verify_First_Bit (0, 16#80#);
      Copy_And_Attach (A, (Referenced_Destination, Fields (1)), Width,
        (Integer_Datum, 1, Ordinary_Integer), V, Status);
      Check (Status = Returned); Verify_First_Bit (1, 16#81#);
      Copy_And_Attach (A, (Referenced_Destination, Fields (1)), Width, Source, V, Status);
      Check (Status = Returned); Verify_First_Bit (1, 16#81#);
      -- Raw replacement remains unsupported: it must not retag the field.
      Before := Snapshot (A);
      Replace_Value (A, 2, Path ("FLD1"), Width, Source, Status);
      Check (Status = Unsupported_Value and then Snapshot (A) = Before);
      Before := Snapshot (A);
      Define_Buffer_Field (A, 2, Path ("FLD1"), Backing, 0, 1, False, Ref, Status);
      Check (Status = Duplicate_Name and then Snapshot (A) = Before);
      Define_Buffer_Field (A, 2, Path ("ZERO"), Backing, 0, 0, True, Ref, Status);
      Check (Status = Invalid_Field_Length and then Snapshot (A) = Before);
      Define_Buffer_Field (A, 2, Path ("OVER"), Backing, 80, 1, True, Ref, Status);
      Check (Status = Buffer_Field_Limit and then Snapshot (A) = Before);
      Define_Buffer_Field (A, 2, Path ("HUGE"), Backing, Integer_Value'Last, Integer_Value'Last, True, Ref, Status);
      Check (Status = Buffer_Field_Limit and then Snapshot (A) = Before);
      Read_Buffer_Field (Foreign, Fields (1), Width, V, Status); Check (Status = Unsupported_Value);
      Write_Buffer_Field (Foreign, Fields (1), Width, (Integer_Datum,1,Ordinary_Integer), Status); Check (Status = Unsupported_Value);
      -- Capture source bytes before overlapping destination writes (offset1bit).
      Define_Buffer_Field (A, 2, Path ("OVLP"), Backing, 1, 64, True, Ref, Status); Check (Status = Returned);
      Write_Buffer_Field (A, Ref, Width, Source, Status); Check (Status = Returned);
      declare Data : constant Bytes := AML_Objects.Byte_Data (Value_Store (Snapshot (A)), Source.Object.ID); begin
         for K in 0 .. 79 loop
            if K in 1 .. 64 then
               Check ((Data (1 + K / 8) / 2 ** (K mod 8)) mod 2 =
                 (Raw (1 + (K - 1) / 8) / 2 ** ((K - 1) mod 8)) mod 2);
            else Check ((Data (1 + K / 8) / 2 ** (K mod 8)) mod 2 =
                 (Raw (1 + K / 8) / 2 ** (K mod 8)) mod 2); end if;
         end loop;
      end;
      Write_Buffer_Field (A, Fields (5), Width, (Integer_Datum,Integer_Value'Last,Ordinary_Integer),Status);
      Check (Status = Returned);
      declare Data : constant Bytes := AML_Objects.Byte_Data (Value_Store (Snapshot (A)), Source.Object.ID); begin
         for I in 1 .. 8 loop Check (Data (I) = (if Width = Bits_32 and I > 4 then 0 else 255)); end loop;
      end;
      -- Rebind source name; fields keep the old buffer identity.
      Replace_Value (A, 0, Path ("BUFF"), Width, (Integer_Datum,7,Ordinary_Integer),Status); Check(Status=Returned);
      Trace_Owner_Roots (A, [], Workspace, Keep, Trace); Check (Trace = Roots_Traced and then Keep (Source.Object.ID));
      Retain (A, (Reference_Datum, Fields (1)), Root, Pinned); Check (Pinned = Retained);
      Trace_Owner_Roots (A, [], Workspace, Keep, Trace); Check (Trace = Roots_Traced and then Keep (Source.Object.ID));
      Release (A, Root, Released_Status); Check (Released_Status = Released);
      -- Package reference closure uses ordinary public reference creation.
      Make_Source(A,Data_Object(Snapshot(A),3),Pkg_Handle,OK);Check(OK);
      Read_Source(A,Pkg_Handle,Pkg,Status);Check(Status=Returned);
      Make_Index(A,Pkg_Handle,0,Pkg_Ref,Status);Check(Status=Returned);
      Store_Reference_Value(A,Pkg_Ref,Width,(Reference_Datum,Fields(1)),Status);Check(Status=Returned);
      Trace_Owner_Roots(A,[Pkg],Workspace,Keep,Trace);
      Check(Trace=Roots_Traced and then Keep(Source.Object.ID) and then Keep(Pkg.Object.ID));
      -- Detached copy sweep, not collection of the live arena. The keep set is
      -- authenticated above; only named refs occur, with no container edge
      -- whose owner is interpreted by Reclamation itself.
      declare
         Store : AML_Objects.State := Value_Store(Snapshot(A));
         Scratch : AML_Objects.Reclamation.Workspace;
         Sweep_Status : AML_Objects.Reclamation.Reclaim_Status;
         Sweep_Owner : AML_Identity.Identity;
         Keep_Copy : AML_Objects.Reclamation.Keep_Set;
         Previous : constant AML_Objects.Usage := AML_Objects.Usage_Of(Store);
      begin
         AML_Identity.Issuer.Issue(Sweep_Owner,OK);Check(OK);
         for J in Keep'Range loop Keep_Copy(J):=Keep(J);end loop;
         AML_Objects.Reclamation.Reclaim(Store,Sweep_Owner,Keep_Copy,Scratch,Sweep_Status);
         Check(Sweep_Status=AML_Objects.Reclamation.Reclaimed);
         Check(AML_Objects.Is_Live(Store,Source.Object.ID));
         Check(AML_Objects.Byte_Data(Store,Source.Object.ID)=
           AML_Objects.Byte_Data(Value_Store(Snapshot(A)),Source.Object.ID));
         Check(AML_Objects.Usage_Of(Store).Objects < Previous.Objects);
      end;
      Before:=Snapshot(A);
      Bad:=Source;Bad.Object.ID:=Pkg.Object.ID;
      Write_Buffer_Field(A,Fields(1),Width,Bad,Status);
      Check(Status=Unsupported_Value and then Snapshot(A)=Before);
      -- Full object pool: no-allocation scalar access succeeds; generic read fails atomically.
      while Values_Used (A).Objects < AML_Objects.Max_Objects loop
         Append (A, Bytes'(1 .. 0 => 0), ID, Alloc); Check (Alloc = AML_Objects.Allocated);
      end loop;
      Read_Buffer_Field (A, Fields (2), Width, V, Status); Check (Status = Returned);
      Write_Buffer_Field (A, Fields (2), Width, (Integer_Datum,42,Ordinary_Integer),Status); Check (Status = Returned);
      Before := Snapshot (A);
      Read_Buffer_Field (A, Fields (6), Width, V, Status); Check (Status = Value_Limit and then Snapshot (A)=Before);
      Check (V.Value_Kind=Integer_Datum and then V.Number=0);
      -- Node quota and field-only roots still validate after object exhaustion.
      for I in 12 .. 23 loop
         declare Name : constant String := "F" & Character'Val (65 + I) & "XX"; begin
            Define_Buffer_Field (A,2,Path(Name),Backing,0,1,False,Ref,Status);Check(Status=Returned);
         end;
      end loop;
      Before:=Snapshot(A);
      Define_Buffer_Field(A,2,Path("FULL"),Backing,0,1,False,Ref,Status);
      Check(Status=Namespace_Limit and then Snapshot(A)=Before);
      Make_Named_Reference (A, 1, Ref, OK);Check(OK);
   end Clock;
   Loaded_Status : Load_Status;
   Result : Execution_Result;
   OK : Boolean;
   V : Datum;
begin
   Reset (Foreign,OK);Check(OK);
   for W in Integer_Width loop
      Width:=W; Reset(A,OK);Check(OK);
      Load(A,Bytes'(16#08#,66,85,70,70,16#11#,13,16#0A#,10)&Raw&
        Bytes'(16#14#,10,84,69,83,84,0,16#5B#,16#33#,16#A4#,0,16#08#,80,75,71,48,16#12#,3,1,0),Width,Loaded_Status);
      Check(Loaded_Status=Loaded);
      Make_Source(A,Data_Object(Snapshot(A),1),Backing,OK);Check(OK);
      Read_Source(A,Backing,Source,Status);Check(Status=Returned);
      Running:=True;Invoke(A,Input,2,[others=><>],0,100,Result);Running:=False;
      Check(Result.Status=Returned);
      Read_Buffer_Field(A,Last_Field,Width,V,Status);Check(Status=Unsupported_Value);
      Trace_Owner_Roots(A,[],Workspace,Keep,Trace);Check(Trace=Roots_Traced and then not Keep(Source.Object.ID));
      Reuse_Only:=True;Running:=True;
      Invoke(A,Input,2,[others=><>],0,100,Result);
      Running:=False;Reuse_Only:=False;Check(Result.Status=Returned);
      Reset(A,OK);Check(OK);
      Read_Buffer_Field(A,Last_Field,Width,V,Status);Check(Status=Unsupported_Value);
   end loop;
   Ada.Text_IO.Put_Line("field owner checks" & Checks'Image);
end Field_Owner_Tests;
