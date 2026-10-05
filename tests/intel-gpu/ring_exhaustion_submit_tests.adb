with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Context_Init;
with Intel_GPU_Live_Ring_Publish;
with Intel_GPU_Application_Submit;
with Intel_GPU_Buffer_Handles;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Buffer_Backing;
procedure Ring_Exhaustion_Submit_Tests is
   -- Real software coordinators, mocked completion and ring RAM, no GPU.
   Owner : Boolean := True;
   Marker : Unsigned_64 := 1;
   Saved_Tail : Unsigned_32 := 384;
   Writes, Quarantines : Natural := 0;
   Pool : Intel_GPU_Buffer_Handles.Registry;
   Name : Intel_GPU_Buffer_Handles.Handle;
   function Owned return Boolean is (Owner);
   procedure Read_Marker (Value : out Unsigned_64; OK : out Boolean) is
   begin Value := Marker; OK := True; end;
   procedure Read_Tail (Value : out Unsigned_32; OK : out Boolean) is
   begin Value := Saved_Tail; OK := True; end;
   procedure Write_Word (Offset, Value : Unsigned_32; OK : out Boolean) is
      pragma Unreferenced (Value);
   begin
      pragma Assert (Offset < 16384 and (Offset >= Saved_Tail or Saved_Tail > 15936));
      Writes := Writes + 1; OK := True;
   end;
   function Publish_Words (Offset, Bytes : Unsigned_32) return Boolean is
     ((Offset = Saved_Tail and Bytes = 384) or else
      (Saved_Tail > 15936 and then
       ((Offset = Saved_Tail and Bytes = 16384 - Saved_Tail) or (Offset = 0 and Bytes = 384))));
   procedure Write_Tail (Value : Unsigned_32; OK : out Boolean) is
   begin Saved_Tail := Value; OK := True; end;
   function Visible return Boolean is (True);
   package Ring is new Intel_GPU_Live_Ring_Publish
     (Owned, Read_Marker, Read_Tail, Write_Word, Publish_Words, Write_Tail, Visible);
   Channel : Ring.Channel;
   Ring_Status : Ring.Result;
   use type Ring.Result;
   function Batch (Handle, GPU, Offset, Bytes : Unsigned_64) return Boolean is
     (Handle = Unsigned_64 (Name) and GPU = 4096 and Offset = 0 and Bytes = 4096);
   type Attempt is record Expected : Unsigned_32 := 0; end record;
   procedure Arm (A : in out Attempt; Previous, Expected : Unsigned_32; OK : out Boolean) is
   begin
      OK := Unsigned_64 (Previous) = Marker;
      A.Expected := Expected;
   end;
   procedure Succeed (OK : out Boolean) is
   begin OK := True; end;
   procedure Publish (GPU : Unsigned_64; Sequence : Unsigned_32; OK : out Boolean) is
   begin
      Ring.Append (Channel, Intel_GPU_ADLN_Context_Init.Build_Batch
        (True, 0, Sequence, GPU), Ring_Status);
      OK := Ring_Status = Ring.Published;
   end;
   procedure Wait_Completion (A : in out Attempt; OK : out Boolean) is
   begin Marker := Unsigned_64 (A.Expected); OK := True; end;
   procedure Quarantine is
   begin
      Owner := False; Quarantines := Quarantines + 1;
      Intel_GPU_Buffer_Handles.Close_Session (Pool, 42);
   end;
   package Submit is new Intel_GPU_Application_Submit
     (Owned, Batch, Attempt, Arm, Succeed, Publish, Succeed, Wait_Completion,
      Succeed, Quarantine);
   State : Submit.State;
   Result : Submit.Result;
   Completion : Unsigned_32;
   Before : Natural;
   Accepted : Boolean;
   use type Submit.Result;
   use type Intel_GPU_Buffer_Handles.Close_Check;
begin
   Intel_GPU_Buffer_Handles.Register (Pool, 42,
     Intel_GPU_Buffer_Reply.From_Linear (16#2000000#,
       Intel_GPU_Buffer_Backing.CPU_Base, 4096, 16#2000000#), Name);
   Submit.Initialize (State, True);
   -- One internal device batch plus forty triangle batches after setup.
   for N in 1 .. 41 loop
      Submit.Execute (State, Unsigned_64 (Name), 4096, 0, 4096, Result, Completion);
      pragma Assert (Result = Submit.Complete and Completion = Unsigned_32 (N + 1));
   end loop;
   pragma Assert (Ring.Tail (Channel) = 16128 and Ring.Sequence (Channel) = 42);
   for N in 42 .. 4097 loop
      Submit.Execute (State, Unsigned_64 (Name), 4096, 0, 4096, Result, Completion);
      pragma Assert (Result = Submit.Complete and Completion = Unsigned_32 (N + 1));
   end loop;
   pragma Assert (Quarantines = 0);
   Intel_GPU_Buffer_Handles.Close (Pool, 42, Name, Accepted);
   pragma Assert (Accepted);
   Before := Writes;
   Owner := False;
   Submit.Execute (State, Unsigned_64 (Name), 4096, 0, 4096, Result, Completion);
   pragma Assert (Result = Submit.Faulted and Writes = Before and Quarantines = 1);
   Ada.Text_IO.Put_Line
     ("Ring exhaustion regression PASS: setup + internal batch +4096 triangles, wrap/reuse and close succeed; later owner loss quarantines (mock GPU)");
end Ring_Exhaustion_Submit_Tests;
