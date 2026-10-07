with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Image_Lease;
with Intel_GPU_Image_Layout;
with Intel_GPU_Buffer_Handles;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Buffer_Backing;
procedure Image_Lease_Tests is
   package L renames Intel_GPU_Image_Lease;
   package H renames Intel_GPU_Buffer_Handles;
   package I renames Intel_GPU_Image_Layout;
   use type L.State, I.Descriptor;
   Pool, Foreign_Pool : H.Registry;
   Source : H.Retained_Reference;
   Object : L.Lease;
   Key : L.Identity := (1, 42, 0, 3, 9, 101, 0, 102);
   Image : constant I.Descriptor := (I.BGRA8_UNorm, I.Linear, 16, 16, 64, 0);
   ID : H.Handle;
   OK : Boolean;
begin
   H.Register (Pool, 42, Intel_GPU_Buffer_Reply.From_Linear
     (16#2000000#, Intel_GPU_Buffer_Backing.CPU_Base, 4096, 16#2000000#), ID);
   H.Retain_Backing (Pool, 42, ID, Source, OK); pragma Assert (OK);
   Key.Allocation := Unsigned_64 (ID);
   L.Prepare (Object, Pool, Source, (Key with delta Session => 43), Image, True, True, OK);
   pragma Assert (not OK and L.Current (Object) = L.Empty);
   L.Prepare (Object, Pool, Source, (Key with delta Allocation => Unsigned_64 (ID) + 1), Image, True, True, OK);
   pragma Assert (not OK and L.Current (Object) = L.Empty);
   L.Prepare (Object, Pool, Source, (Key with delta Allocation => Unsigned_64'Last), Image, True, True, OK);
   pragma Assert (not OK and L.Current (Object) = L.Empty);
   L.Prepare (Object, Pool, Source, Key, Image, False, True, OK);
   pragma Assert (not OK and L.Current (Object) = L.Empty);
   L.Prepare (Object, Pool, Source, Key, Image, True, False, OK);
   pragma Assert (not OK);
   L.Prepare (Object, Pool, Source, Key, (Image with delta Height => 1000), True, True, OK);
   pragma Assert (not OK);
   L.Prepare (Object, Pool, Source, (Key with delta Serial => 0), Image, True, True, OK);
   pragma Assert (not OK);
   L.Prepare (Object, Pool, Source, (Key with delta Display_Instance => 0), Image, True, True, OK);
   pragma Assert (not OK);
   L.Prepare (Object, Pool, Source, (Key with delta Consumer_Instance => 0), Image, True, True, OK);
   pragma Assert (not OK);
   L.Prepare (Object, Foreign_Pool, Source, Key, Image, True, True, OK);
   pragma Assert (not OK);
   H.Close (Pool, 42, ID, OK); pragma Assert (OK);
   L.Prepare (Object, Pool, Source, Key, Image, True, True, OK);
   pragma Assert (OK and L.Current (Object) = L.Held);
   pragma Assert (H.Writes_Excluded (Pool, 42, ID));
   pragma Assert (H.Session_Writes_Excluded (Pool, 42));
   pragma Assert (not H.Session_Writes_Excluded (Pool, 43));
   H.Return_Reference (Pool, Source, True, OK); pragma Assert (OK);
   pragma Assert (L.Backing (Object, Pool, Key).Ready and L.Layout (Object, Key) = Image);
   pragma Assert (not L.Backing (Object, Pool, (Key with delta Output_Epoch => 4)).Ready);
   pragma Assert (not H.Can_Release_Backing (Pool, 42, ID));
   for Changed in 1 .. 3 loop
      declare
         Other_Key : L.Identity := Key;
      begin
         case Changed is
            when 1 => Other_Key.Output_Number := 1;
            when 2 => Other_Key.Display_Instance := Key.Display_Instance + 1;
            when others => Other_Key.Consumer_Instance := Key.Consumer_Instance + 1;
         end case;
         pragma Assert (not L.Backing (Object, Pool, Other_Key).Ready);
         L.Retire (Object, Pool, Other_Key, True, True, True, OK);
         pragma Assert (not OK and H.Writes_Excluded (Pool, 42, ID));
      end;
   end loop;
   L.Retire (Object, Pool, (Key with delta Serial => 10), True, True, True, OK);
   pragma Assert (not OK);
   for GPU in Boolean loop
      for CPU in Boolean loop
         for Display in Boolean loop
            if not (GPU and CPU and Display) then
               L.Retire (Object, Pool, Key, GPU, CPU, Display, OK);
               pragma Assert (not OK and L.Current (Object) = L.Held);
               pragma Assert (not H.Can_Release_Backing (Pool, 42, ID));
            end if;
         end loop;
      end loop;
   end loop;
   L.Retire (Object, Foreign_Pool, Key, True, True, True, OK);
   pragma Assert (not OK and L.Current (Object) = L.Held);
   L.Retire (Object, Pool, Key, True, True, True, OK);
   pragma Assert (OK and L.Current (Object) = L.Retired);
   pragma Assert (not H.Writes_Excluded (Pool, 42, ID));
   pragma Assert (not H.Session_Writes_Excluded (Pool, 42));
   pragma Assert (H.Can_Release_Backing (Pool, 42, ID));
   pragma Assert (not L.Backing (Object, Pool, Key).Ready);
   L.Retire (Object, Pool, Key, True, True, True, OK); pragma Assert (not OK);
   L.Prepare (Object, Pool, Source, Key, Image, True, True, OK); pragma Assert (not OK);
   declare
      Failed_Pool : H.Registry;
      Failed_Source : H.Retained_Reference;
      Held_Lease : L.Lease;
      Name : H.Handle;
      Failed_Key : L.Identity := (2, 51, 0, 8, 11, 101, 0, 102);
   begin
      H.Register (Failed_Pool, 51, Intel_GPU_Buffer_Reply.From_Linear
        (16#2000000#, Intel_GPU_Buffer_Backing.CPU_Base, 4096, 16#2000000#), Name);
      Failed_Key.Allocation := Unsigned_64 (Name);
      H.Retain_Backing (Failed_Pool, 51, Name, Failed_Source, OK);
      pragma Assert (OK);
      L.Prepare (Held_Lease, Failed_Pool, Failed_Source, Failed_Key, Image, True, True, OK);
      pragma Assert (OK);
      H.Close (Failed_Pool, 51, Name, OK); pragma Assert (OK);
      H.Return_Reference (Failed_Pool, Failed_Source, True, OK); pragma Assert (OK);
      H.Quarantine (Failed_Pool);
      pragma Assert (not L.Backing (Held_Lease, Failed_Pool, Failed_Key).Ready);
      L.Retire (Held_Lease, Failed_Pool, Failed_Key, True, True, True, OK);
      pragma Assert (not OK and L.Current (Held_Lease) = L.Held);
      H.Release_Retired_Backing (Failed_Pool, 51, Name, True, OK);
      pragma Assert (not OK);
      L.Retire (Held_Lease, Failed_Pool, Failed_Key, True, True, True, OK);
      pragma Assert (not OK and L.Current (Held_Lease) = L.Held);
   end;
   declare
      Root, First, Derived : H.Retained_Reference;
      Pins : H.Registry;
      Name : H.Handle;
   begin
      H.Register (Pins, 71, Intel_GPU_Buffer_Reply.From_Linear
        (16#2000000#, Intel_GPU_Buffer_Backing.CPU_Base, 4096, 16#2000000#), Name);
      H.Retain_Backing (Pins, 71, Name, Root, OK); pragma Assert (OK);
      H.Retain_Referenced_Backing (Pins, Root, First, OK, Exclude_Writes => True);
      pragma Assert (OK and H.Writes_Excluded (Pins, 71, Name));
      H.Retain_Referenced_Backing (Pins, First, Derived, OK);
      pragma Assert (OK);
      H.Close (Pins, 71, Name, OK); pragma Assert (OK);
      H.Return_Reference (Pins, Root, True, OK); pragma Assert (OK);
      H.Return_Reference (Pins, First, True, OK); pragma Assert (OK);
      pragma Assert (H.Writes_Excluded (Pins, 71, Name));
      H.Return_Reference (Pins, First, True, OK); pragma Assert (not OK);
      H.Return_Reference (Foreign_Pool, Derived, True, OK); pragma Assert (not OK);
      pragma Assert (H.Writes_Excluded (Pins, 71, Name));
      H.Return_Reference (Pins, Derived, False, OK); pragma Assert (not OK);
      pragma Assert (H.Session_Writes_Excluded (Pins, 71));
      H.Return_Reference (Pins, Derived, True, OK); pragma Assert (OK);
      pragma Assert (not H.Writes_Excluded (Pins, 71, Name));
      pragma Assert (H.Can_Release_Backing (Pins, 71, Name));
   end;
   declare
      Shared : H.Registry;
      A, B, Frozen_A, Frozen_B : H.Retained_Reference;
      Name_A, Name_B : H.Handle;
   begin
      pragma Assert (not H.Session_Writes_Excluded (Shared, 80));
      pragma Assert (H.Session_Writes_Excluded (Shared, 0));
      H.Register (Shared, 80, Intel_GPU_Buffer_Reply.From_Linear
        (16#2000000#, Intel_GPU_Buffer_Backing.CPU_Base, 4096, 16#2000000#), Name_A);
      H.Register (Shared, 81, Intel_GPU_Buffer_Reply.From_Linear
        (16#2001000#, Intel_GPU_Buffer_Backing.CPU_Base + 4096, 4096, 16#2000000#), Name_B);
      H.Retain_Backing (Shared, 80, Name_A, A, OK); pragma Assert (OK);
      H.Retain_Backing (Shared, 81, Name_B, B, OK); pragma Assert (OK);
      H.Retain_Referenced_Backing (Shared, A, Frozen_A, OK, True); pragma Assert (OK);
      H.Retain_Referenced_Backing (Shared, B, Frozen_B, OK, True); pragma Assert (OK);
      pragma Assert (H.Session_Writes_Excluded (Shared, 80));
      pragma Assert (H.Session_Writes_Excluded (Shared, 81));
      pragma Assert (not H.Session_Writes_Excluded (Shared, 82));
      H.Return_Reference (Shared, Frozen_A, True, OK); pragma Assert (OK);
      pragma Assert (not H.Session_Writes_Excluded (Shared, 80));
      pragma Assert (H.Session_Writes_Excluded (Shared, 81));
      H.Return_Reference (Shared, Frozen_A, True, OK); pragma Assert (not OK);
      pragma Assert (H.Session_Writes_Excluded (Shared, 81));
      H.Return_Reference (Shared, Frozen_B, True, OK); pragma Assert (OK);
      pragma Assert (not H.Session_Writes_Excluded (Shared, 81));
      H.Return_Reference (Shared, A, True, OK); pragma Assert (OK);
      H.Return_Reference (Shared, B, True, OK); pragma Assert (OK);
      H.Quarantine (Shared);
      pragma Assert (H.Session_Writes_Excluded (Shared, 80));
   end;
   Ada.Text_IO.Put_Line ("Session hold accounting PASS: independent owners, unrelated admission, exact final return, fail-closed quarantine");
   Ada.Text_IO.Put_Line ("Image write exclusion PASS: inherited hold survives closure, parent return and rejected returns");
   Ada.Text_IO.Put_Line ("Image lease PASS: closed producer, independent pin, exact identity, all drain domains, no local reuse");
   Ada.Text_IO.Put_Line ("Image lease quarantine PASS: backing inaccessible, failed return retains lease, repeated retirement cannot release");
end Image_Lease_Tests;
