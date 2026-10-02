with Interfaces; use Interfaces;
with CuBit.Messages;
with Native_GPU_Buffers;
with Ada.Text_IO;
procedure Buffer_Bridge_Tests is
   package M renames CuBit.Messages;
   use type M.MessageWords;
   ID : aliased Unsigned_32 := 999;
   Mapping : aliased Unsigned_32 := 999;
   Reference : aliased Unsigned_64 := 999;
   Status : Unsigned_32;
   function Test_C return Unsigned_32
     with Import, Convention => C, External_Name => "test_buffer_c_bridge";
begin
   declare
      Before : constant Natural := M.Calls;
   begin
      pragma Assert (Native_GPU_Buffers.Memory_Contract (64) = 0);
      pragma Assert (Native_GPU_Buffers.Memory_Contract (Unsigned_64'Last) = 0);
      pragma Assert (M.Calls = Before);
      for Policy in Unsigned_64 range 1 .. 2 loop
         M.Memory_Response := [0, 1, Policy, 0];
         pragma Assert (Native_GPU_Buffers.Memory_Contract (63) = Unsigned_32 (Policy));
      end loop;
      for Field in 0 .. 3 loop
         for Bit in 0 .. 63 loop
            M.Memory_Response := [0, 1, 2, 0];
            M.Memory_Response (Field) := M.Memory_Response (Field) xor
              Shift_Left (Unsigned_64'(1), Bit);
            pragma Assert (Native_GPU_Buffers.Memory_Contract (63) = 0);
         end loop;
      end loop;
      M.Memory_Response := [0, 1, 2, 0];
      for Fault in 1 .. 10 loop
         if Fault /= 5 then
            M.Fault := Fault;
            pragma Assert (Native_GPU_Buffers.Memory_Contract (63) = 0);
         end if;
      end loop;
      M.Fault := 0;
   end;
   declare
      Before : constant Natural := M.Calls;
   begin
      pragma Assert (Native_GPU_Buffers.Poll_Session_Retirement (64) = 5);
      pragma Assert (Native_GPU_Buffers.Poll_Session_Retirement (Unsigned_64'Last) = 5);
      pragma Assert (M.Calls = Before);
      for Code in Unsigned_64 range 0 .. 4 loop
         M.Retirement_Response := [Code, 1, 0, 0];
         pragma Assert (Native_GPU_Buffers.Poll_Session_Retirement (63) = Unsigned_32 (Code));
      end loop;
      for Field in 0 .. 3 loop
         M.Retirement_Response := [0, 1, 0, 0];
         M.Retirement_Response (Field) := (if Field = 0 then 5 else 2);
         pragma Assert (Native_GPU_Buffers.Poll_Session_Retirement (63) = 5);
      end loop;
      M.Retirement_Response := [0, 1, 0, 0];
      for Fault in 1 .. 10 loop
         if Fault /= 5 then
            M.Fault := Fault;
            pragma Assert (Native_GPU_Buffers.Poll_Session_Retirement (63) = 5);
         end if;
      end loop;
      M.Fault := 0;
      pragma Assert (Native_GPU_Buffers.Poll_Session_Retirement (62) = 1);
   end;
   declare
      Tag : aliased Unsigned_64 := 999;
      Before : constant Natural := M.Calls;
      Expected : constant Unsigned_64 := 16#4750_0000_0000_0001#;
   begin
      Status := Native_GPU_Buffers.Close_Session (64, Tag'Access);
      pragma Assert (Status = 4 and Tag = 0 and M.Calls = Before);
      Status := Native_GPU_Buffers.Close_Session (63, null);
      pragma Assert (Status = 4 and M.Calls = Before);
      Status := Native_GPU_Buffers.Close_Session (63, Tag'Access);
      pragma Assert (Status = 0 and Tag = Expected);
      for Bad_Field in 0 .. 3 loop
         M.Close_Response := [0, 1, Expected, 0];
         M.Close_Response (Bad_Field) :=
           (case Bad_Field is when 0 => 4, when 1 => 2,
            when 2 => 0, when others => 1);
         Tag := 999;
         Status := Native_GPU_Buffers.Close_Session (63, Tag'Access);
         pragma Assert (Status = 4 and Tag = 0);
      end loop;
      for Code in Unsigned_64 range 1 .. 3 loop
         M.Close_Response := [Code, 1, 0, 0];
         Status := Native_GPU_Buffers.Close_Session (63, Tag'Access);
         pragma Assert (Status = Unsigned_32 (Code) and Tag = 0);
         M.Close_Response (2) := Expected;
         Status := Native_GPU_Buffers.Close_Session (63, Tag'Access);
         pragma Assert (Status = 4 and Tag = 0);
      end loop;
      M.Close_Response := [0, 1, Expected, 0];
      for Fault in 1 .. 10 loop
         if Fault in 1 | 2 | 3 | 6 | 7 | 8 | 9 | 10 then
            M.Fault := Fault;
            Status := Native_GPU_Buffers.Close_Session (63, Tag'Access);
            pragma Assert (Status = 4 and Tag = 0);
         end if;
      end loop;
      M.Fault := 0;
   end;
   declare
      Generation : aliased Unsigned_32 := 999;
      type Values is array (Positive range <>) of Unsigned_64;
      Before : Natural;
   begin
      for Remove in Unsigned_32 range 0 .. 1 loop
         M.Update_Response := [0, 1, 8, 0];
         Status := Native_GPU_Buffers.Update_Binding
           (63, 17, 16#20000#, 4096, 8192, Remove, 7, Generation'Access);
         pragma Assert (Status = 0 and Generation = 8);
         pragma Assert (M.Update_Request =
           [1 + Unsigned_64 (Remove) * 2 ** 16 + 7 * 2 ** 32,
            17 + 2 ** 32, 16#20000#, 8192]);
      end loop;
      M.Update_Response := [0, 1, Unsigned_64 (Unsigned_32'Last), 0];
      Status := Native_GPU_Buffers.Update_Binding
        (63, 1, 4096, 0, 4096, 0, Unsigned_32'Last - 1, Generation'Access);
      pragma Assert (Status = 0 and Generation = Unsigned_32'Last);
      M.Update_Response := [0, 1, 1, 0];
      for Fault in 1 .. 10 loop
         if Fault /= 5 then
            M.Fault := Fault;
            Status := Native_GPU_Buffers.Update_Binding
              (63, 1, 4096, 0, 4096, 0, 0, Generation'Access);
            pragma Assert (Status = 4 and Generation = 0);
         end if;
      end loop;
      M.Fault := 0;
      for Value of Values'(0, 7, 9, 2 ** 32, Unsigned_64'Last) loop
         M.Update_Response := [0, 1, Value, 0];
         Status := Native_GPU_Buffers.Update_Binding
           (63, 1, 4096, 0, 4096, 0, 7, Generation'Access);
         pragma Assert (Status = 4 and Generation = 0);
      end loop;
      for Code in Unsigned_64 range 1 .. 3 loop
         M.Update_Response := [Code, 1, 0, 0];
         Status := Native_GPU_Buffers.Update_Binding
           (63, 1, 4096, 0, 4096, 0, 0, Generation'Access);
         pragma Assert (Status = Unsigned_32 (Code) and Generation = 0);
         M.Update_Response (2) := 1;
         Status := Native_GPU_Buffers.Update_Binding
           (63, 1, 4096, 0, 4096, 0, 0, Generation'Access);
         pragma Assert (Status = 4 and Generation = 0);
      end loop;
      M.Update_Response := [0, 1, 1, 1];
      Status := Native_GPU_Buffers.Update_Binding
        (63, 1, 4096, 0, 4096, 0, 0, Generation'Access);
      pragma Assert (Status = 4 and Generation = 0);
      Before := M.Calls;
      for Value of Values'(0, 1, 2 ** 48, Unsigned_64'Last) loop
         Status := Native_GPU_Buffers.Update_Binding
           (63, 1, Value, 0, 4096, 0, 0, Generation'Access);
         pragma Assert (Status = 4 and Generation = 0 and M.Calls = Before);
      end loop;
      for Value of Values'(0, 1, 16 * 1024 * 1024 + 4096, Unsigned_64'Last) loop
         Status := Native_GPU_Buffers.Update_Binding
           (63, 1, 4096, 0, Value, 0, 0, Generation'Access);
         pragma Assert (Status = 4 and Generation = 0 and M.Calls = Before);
      end loop;
      for Value of Values'(1, 16 * 1024 * 1024, Unsigned_64'Last) loop
         Status := Native_GPU_Buffers.Update_Binding
           (63, 1, 4096, Value, 4096, 0, 0, Generation'Access);
         pragma Assert (Status = 4 and Generation = 0 and M.Calls = Before);
      end loop;
      Status := Native_GPU_Buffers.Update_Binding
        (63, 1, 2 ** 48 - 4096, 0, 8192, 0, 0, Generation'Access);
      pragma Assert (Status = 4 and M.Calls = Before);
      Status := Native_GPU_Buffers.Update_Binding
        (64, 1, 4096, 0, 4096, 0, 0, Generation'Access);
      pragma Assert (Status = 4 and M.Calls = Before);
      Status := Native_GPU_Buffers.Update_Binding
        (63, 0, 4096, 0, 4096, 0, 0, Generation'Access);
      pragma Assert (Status = 4 and M.Calls = Before);
      Status := Native_GPU_Buffers.Update_Binding
        (63, 1, 4096, 0, 4096, 2, 0, Generation'Access);
      pragma Assert (Status = 4 and M.Calls = Before);
      Status := Native_GPU_Buffers.Update_Binding
        (63, 1, 4096, 0, 4096, 0, Unsigned_32'Last, Generation'Access);
      pragma Assert (Status = 4 and M.Calls = Before);
      Status := Native_GPU_Buffers.Update_Binding
        (63, 1, 4096, 0, 4096, 0, 0, null);
      pragma Assert (Status = 4 and M.Calls = Before);
      M.Update_Response := [0, 1, 1, 0];
   end;
   declare
      Completion : aliased Unsigned_32 := 999;
      type Values is array (Positive range <>) of Unsigned_64;
      Before : Natural;
   begin
      Status := Native_GPU_Buffers.Submit (63, 1, 16#20000#, 8, 4096, 1, Completion'Access);
      pragma Assert (Status = 0 and Completion = 2);
      pragma Assert (M.Submit_Request = [1 + 8 * 2 ** 32, 1, 16#20000#, 4096]);
      for Sequence in Unsigned_32 range 3 .. 20 loop
         M.Submit_Response := [0, 1, Unsigned_64 (Sequence), 0];
         Before := M.Calls;
         Status := Native_GPU_Buffers.Submit
           (63, 1, 16#20000#, 8, 4096, Sequence - 1, Completion'Access);
         pragma Assert (Status = 0 and Completion = Sequence and M.Calls = Before + 1);
      end loop;
      M.Submit_Response := [0, 1, Unsigned_64 (Unsigned_32'Last), 0];
      Status := Native_GPU_Buffers.Submit
        (63, 1, 16#20000#, 8, 4096, Unsigned_32'Last - 1, Completion'Access);
      pragma Assert (Status = 0 and Completion = Unsigned_32'Last);
      M.Submit_Response := [0, 1, 2, 0];
      for Fault in 1 .. 10 loop
         if Fault /= 5 then
            M.Fault := Fault; Before := M.Calls;
            Status := Native_GPU_Buffers.Submit (63, 1, 16#20000#, 8, 4096, 1, Completion'Access);
            pragma Assert (Status = 4 and Completion = 0 and M.Calls = Before + 1);
         end if;
      end loop;
      M.Fault := 0;
      for Marker of Values'(0, 1, 3, 2 ** 32, Unsigned_64'Last) loop
         M.Submit_Response := [0, 1, Marker, 0];
         Status := Native_GPU_Buffers.Submit (63, 1, 16#20000#, 8, 4096, 1, Completion'Access);
         pragma Assert (Status = 4 and Completion = 0);
      end loop;
      for Code in Unsigned_64 range 1 .. 3 loop
         M.Submit_Response := [Code, 1, 0, 0];
         Status := Native_GPU_Buffers.Submit (63, 1, 16#20000#, 8, 4096, 1, Completion'Access);
         pragma Assert (Status = Unsigned_32 (Code) and Completion = 0);
      end loop;
      M.Submit_Response := [0, 1, 2, 1];
      Status := Native_GPU_Buffers.Submit (63, 1, 16#20000#, 8, 4096, 1, Completion'Access);
      pragma Assert (Status = 4 and Completion = 0);
      Before := M.Calls;
      for GPU of Values'(0, 1, 2 ** 48 - 8, 2 ** 48, Unsigned_64'Last) loop
         Status := Native_GPU_Buffers.Submit (63, 1, GPU, 0, 4096, 1, Completion'Access);
         pragma Assert (Status = 4 and Completion = 0 and M.Calls = Before);
      end loop;
      for Bytes of Values'(0, 1, 16 * 1024 * 1024 + 4, Unsigned_64'Last) loop
         Status := Native_GPU_Buffers.Submit (63, 1, 16#20000#, 0, Bytes, 1, Completion'Access);
         pragma Assert (Status = 4 and Completion = 0 and M.Calls = Before);
      end loop;
      for Offset of Values'(1, 16 * 1024 * 1024, Unsigned_64'Last - 7) loop
         Status := Native_GPU_Buffers.Submit (63, 1, 16#20000#, Offset, 4096, 1, Completion'Access);
         pragma Assert (Status = 4 and Completion = 0 and M.Calls = Before);
      end loop;
      Status := Native_GPU_Buffers.Submit (64, 1, 16#20000#, 0, 4096, 1, Completion'Access);
      pragma Assert (Status = 4 and M.Calls = Before);
      Status := Native_GPU_Buffers.Submit (63, 0, 16#20000#, 0, 4096, 1, Completion'Access);
      pragma Assert (Status = 4 and M.Calls = Before);
      Status := Native_GPU_Buffers.Submit (63, 1, 16#20000#, 0, 4096, 0, Completion'Access);
      pragma Assert (Status = 4 and M.Calls = Before);
      Status := Native_GPU_Buffers.Submit (63, 1, 16#20000#, 0, 4096, Unsigned_32'Last, Completion'Access);
      pragma Assert (Status = 4 and M.Calls = Before);
      Status := Native_GPU_Buffers.Submit (63, 1, 16#20000#, 0, 4096, 1, null);
      pragma Assert (Status = 4 and M.Calls = Before);
      M.Submit_Response := [0, 1, 2, 0];
   end;
   M.Map_Response := [0, 1, 1, 9 * 2 ** 32 + 8];
   Status := Native_GPU_Buffers.Map_Presentation
     (63, 1, 4096, 4096, Mapping'Access, Reference'Access);
   pragma Assert (Status = 0 and Mapping = 1 and Reference = 9 * 2 ** 32 + 8);
   pragma Assert (M.Map_Request = [1 + 3 * 2 ** 32, 1, 4096, 4096]);
   for Fault in 1 .. 10 loop
      M.Fault := Fault;
      Status := Native_GPU_Buffers.Map_Presentation
        (63, 1, 4096, 4096, Mapping'Access, Reference'Access);
      pragma Assert (Status = 5 and Mapping = 0 and Reference = 0);
   end loop;
   M.Fault := 0;
   declare
      Before : constant Natural := M.Calls;
   begin
      Status := Native_GPU_Buffers.Map
        (63, 1, 4096, 4096, 3, Mapping'Access, Reference'Access);
      pragma Assert (Status = 5 and Mapping = 0 and Reference = 0 and M.Calls = Before);
      Status := Native_GPU_Buffers.Map_Presentation
        (64, 1, 4096, 4096, Mapping'Access, Reference'Access);
      pragma Assert (Status = 5 and Mapping = 0 and Reference = 0 and M.Calls = Before);
   end;
   M.Calls := 0;
   M.Map_Response := [0, 1, 1, 7 * 2 ** 32 + 8];
   declare
      Before : constant Natural := M.Calls;
   begin
      Status := Native_GPU_Buffers.Prepare_Context (64);
      pragma Assert (Status = 4 and M.Calls = Before);
      Status := Native_GPU_Buffers.Prepare_Context (Unsigned_64'Last);
      pragma Assert (Status = 4 and M.Calls = Before);
      Status := Native_GPU_Buffers.Register_Context (64);
      pragma Assert (Status = 4 and M.Calls = Before);
      Status := Native_GPU_Buffers.Register_Context (Unsigned_64'Last);
      pragma Assert (Status = 4 and M.Calls = Before);
   end;
   Status := Native_GPU_Buffers.Create (64, 4096, ID'Access);
   pragma Assert (Status = 4 and ID = 0 and M.Calls = 0);
   Status := Native_GPU_Buffers.Create (63, 1, ID'Access);
   pragma Assert (Status = 4 and M.Calls = 0);
   Status := Native_GPU_Buffers.Create (63, 4096, null);
   pragma Assert (Status = 4 and M.Calls = 0);
   -- Malformed replies cannot become successful preparation. This fixture
   -- only checks transport/ABI; it does not execute native publication.
   for Code in Unsigned_64 range 0 .. 3 loop
      M.Prepare_Response := [Code, 1, 0, 0];
      Status := Native_GPU_Buffers.Prepare_Context (63);
      pragma Assert (Status = Unsigned_32 (Code));
   end loop;
   for Field in 0 .. 3 loop
      M.Prepare_Response := [0, 1, 0, 0];
      M.Prepare_Response (Field) := Unsigned_64'Last;
      Status := Native_GPU_Buffers.Prepare_Context (63);
      pragma Assert (Status = 4);
   end loop;
   M.Prepare_Response := [0, 1, 0, 0];
   for Fault in 1 .. 4 loop
      M.Fault := Fault;
      Status := Native_GPU_Buffers.Prepare_Context (63);
      pragma Assert (Status = 4);
   end loop;
   M.Fault := 0;
   -- Independent label/reply checks: registration is not preparation, and
   -- neither fixture result establishes execution readiness.
   for Code in Unsigned_64 range 0 .. 3 loop
      M.Register_Response := [Code, 1, 0, 0];
      Status := Native_GPU_Buffers.Register_Context (63);
      pragma Assert (Status = Unsigned_32 (Code));
   end loop;
   for Field in 0 .. 3 loop
      M.Register_Response := [0, 1, 0, 0];
      M.Register_Response (Field) := Unsigned_64'Last;
      Status := Native_GPU_Buffers.Register_Context (63);
      pragma Assert (Status = 4);
   end loop;
   M.Register_Response := [0, 1, 0, 0];
   for Fault in 1 .. 10 loop
      if Fault /= 5 then -- zero word3 is canonical for these transitions
         M.Fault := Fault;
         declare
            Before : constant Natural := M.Calls;
         begin
            Status := Native_GPU_Buffers.Register_Context (63);
            pragma Assert (Status = 4 and M.Calls = Before + 1);
         end;
         Status := Native_GPU_Buffers.Prepare_Context (63);
         pragma Assert (Status = 4);
      end if;
   end loop;
   M.Fault := 0;
   Status := Native_GPU_Buffers.Register_Context (62);
   pragma Assert (Status = 1);
   Status := Native_GPU_Buffers.Prepare_Context (62);
   pragma Assert (Status = 1);
   Status := Native_GPU_Buffers.Create (62, 4096, ID'Access);
   pragma Assert (Status = 1 and ID = 0);
   Status := Native_GPU_Buffers.Create (63, 4096, ID'Access);
   pragma Assert (Status = 0 and ID = 1);
   declare
      Before : constant Natural := M.Calls;
   begin
      Status := Native_GPU_Buffers.Bind_GPU (64, ID, 4096, 0, 4096);
      pragma Assert (Status = 4 and M.Calls = Before);
      Status := Native_GPU_Buffers.Bind_GPU (63, ID, 2 ** 48 - 4096, 0, 8192);
      pragma Assert (Status = 4 and M.Calls = Before);
      Status := Native_GPU_Buffers.Bind_GPU (63, ID, 4096, 1, 4096);
      pragma Assert (Status = 4 and M.Calls = Before);
      Status := Native_GPU_Buffers.Bind_GPU (63, ID, 4096, 16 * 1024 * 1024, 4096);
      pragma Assert (Status = 4 and M.Calls = Before);
      Status := Native_GPU_Buffers.Bind_GPU (63, ID, 4096, Unsigned_64'Last, 4096);
      pragma Assert (Status = 4 and M.Calls = Before);
      Status := Native_GPU_Buffers.Bind_GPU (62, ID, 4096, 0, 4096);
      pragma Assert (Status = 1);
      Status := Native_GPU_Buffers.Bind_GPU (63, ID, 4096, 0, 4096);
      pragma Assert (Status = 0);
      Status := Native_GPU_Buffers.Bind_GPU (63, ID, 4096, 0, 4096);
      pragma Assert (Status = 1);
      Status := Native_GPU_Buffers.Unbind_GPU (64, ID, 4096, 0, 4096);
      pragma Assert (Status = 4);
      Status := Native_GPU_Buffers.Unbind_GPU (62, ID, 4096, 0, 4096);
      pragma Assert (Status = 1);
      Status := Native_GPU_Buffers.Unbind_GPU (63, ID, 4096, 4096, 4096);
      pragma Assert (Status = 1);
      Status := Native_GPU_Buffers.Unbind_GPU (63, ID, 4096, 0, 4096);
      pragma Assert (Status = 0);
      Status := Native_GPU_Buffers.Unbind_GPU (63, ID, 4096, 0, 4096);
      pragma Assert (Status = 1);
      Status := Native_GPU_Buffers.Bind_GPU (63, ID, 4096, 0, 4096);
      pragma Assert (Status = 0);
      for Fault in 1 .. 7 loop
         M.Fault := Fault;
         Status := Native_GPU_Buffers.Bind_GPU (63, ID, Unsigned_64 (Fault + 1) * 4096, 0, 4096);
         pragma Assert (Status = 4);
      end loop;
      M.Fault := 0;
      for Fault in 1 .. 7 loop
         M.Fault := Fault;
         Status := Native_GPU_Buffers.Unbind_GPU
           (63, ID, Unsigned_64 (Fault + 1) * 4096, 0, 4096);
         pragma Assert (Status = 4);
      end loop;
      M.Fault := 0;
   end;
   Status := Native_GPU_Buffers.Close (62, ID);
   pragma Assert (Status = 1);
   Status := Native_GPU_Buffers.Close (63, ID);
   pragma Assert (Status = 0);
   Status := Native_GPU_Buffers.Close (63, ID);
   pragma Assert (Status = 1);
   Status := Native_GPU_Buffers.Create (63, 8192, ID'Access);
   pragma Assert (Status = 0);
   Status := Native_GPU_Buffers.Bind_GPU (63, ID, 16#20000#, 4096, 4096);
   pragma Assert (Status = 0);
   pragma Assert (M.Bound_DMA = M.Last_Allocation_DMA + 4096);
   -- Offset is checked against this BO, not only the protocol maximum.
   Status := Native_GPU_Buffers.Bind_GPU (63, ID, 16#21000#, 8192, 4096);
   pragma Assert (Status = 1 and M.Bound_DMA = 0);
   Status := Native_GPU_Buffers.Bind_GPU (63, ID, 16#21000#, 0, 8192);
   pragma Assert (Status = 0 and M.Bound_DMA = M.Last_Allocation_DMA);
   Status := Native_GPU_Buffers.Close (63, ID);
   pragma Assert (Status = 0);
   for Fault in 1 .. 7 loop
      M.Fault := Fault;
      ID := 999;
      Status := Native_GPU_Buffers.Create (63, 4096, ID'Access);
      pragma Assert (Status = 4 and ID = 0);
   end loop;
   M.Fault := 0;
   declare
      Before : constant Natural := M.Calls;
      use type M.MessageWords;
   begin
      Status := Native_GPU_Buffers.Map (63, 1, 0, 4096, 2, Mapping'Access, Reference'Access);
      pragma Assert (Status = 5 and Mapping = 0 and Reference = 0 and M.Calls = Before);
      Status := Native_GPU_Buffers.Map
        (63, 1, Unsigned_64'Last - 4095, 4096, 1, Mapping'Access, Reference'Access);
      pragma Assert (Status = 5 and M.Calls = Before);
      Status := Native_GPU_Buffers.Map (63, 1, 0, 4096, 0, Mapping'Access, Reference'Access);
      pragma Assert (Status = 0 and Mapping = 1 and Reference = 7 * 2 ** 32 + 8);
      pragma Assert (M.Map_Request = [1, 1, 0, 4096]);
      for Fault in 1 .. 7 loop
         M.Fault := Fault;
         Status := Native_GPU_Buffers.Map (63, 1, 0, 4096, 1, Mapping'Access, Reference'Access);
         pragma Assert (Status = 5 and Mapping = 0 and Reference = 0);
      end loop;
      M.Fault := 0;
      M.Map_Response := [0, 1, 1, 7 * 2 ** 32 + 4096];
      Status := Native_GPU_Buffers.Map (63, 1, 0, 4096, 1, Mapping'Access, Reference'Access);
      pragma Assert (Status = 5 and Mapping = 0 and Reference = 0);
      M.Map_Response := [4, 1, 0, 0];
      Status := Native_GPU_Buffers.Map (63, 1, 0, 4096, 1, Mapping'Access, Reference'Access);
      pragma Assert (Status = 5);
      Status := Native_GPU_Buffers.Retire_Map (63, 1);
      pragma Assert (Status = 4 and M.Map_Request = [1 + 2 * 2 ** 32, 1, 0, 0]);
      M.Map_Response := [0, 1, 0, 0];
      Status := Native_GPU_Buffers.Retire_Map (63, 1);
      pragma Assert (Status = 0);
      M.Map_Response := [0, 1, 1, 0];
      Status := Native_GPU_Buffers.Retire_Map (63, 1);
      pragma Assert (Status = 5);
   end;
   pragma Assert (Test_C = 0);
   Ada.Text_IO.Put_Line
     ("GPU buffer bridge PASS: C/Ada ABI, composed service, malformed replies");
end Buffer_Bridge_Tests;
