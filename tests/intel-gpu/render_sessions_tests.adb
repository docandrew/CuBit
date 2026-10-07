with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Render_Sessions; use Intel_GPU_Render_Sessions;
with Intel_GPU_Render_Sessions.Testing;
with Intel_GPU_Buffer_Handles;
with Intel_GPU_Buffer_Backing;
with Intel_GPU_Buffer_Reply;
procedure Render_Sessions_Tests is
   Object : Registry;
   Tag, Old_Tag : Unsigned_64;
   OK : Boolean;
begin
   Intel_GPU_Render_Sessions.Testing.Run;
   pragma Assert (Issued_Tag (Object, 0) = 0);
   for I in 1 .. Capacity loop
      pragma Assert (Issued_Tag (Object, I) = 0);
   end loop;
   pragma Assert (Storage_Index (Object, Tag_Base + 1) = 0);
   pragma Assert (Storage_Index (Object, 0) = 0);
   pragma Assert (Storage_Index (Object, Unsigned_64'Last) = 0);
   Reserve (Object, 0, Tag); pragma Assert (Tag = 0);
   for I in 1 .. Capacity loop
      pragma Assert (Storage_Index (Object, Tag_Base + Unsigned_64 (I)) = 0);
      Reserve (Object, 42, Tag); pragma Assert (Tag = Tag_Base + Unsigned_64 (I));
      pragma Assert (Storage_Index (Object, Tag) = I);
      pragma Assert (Issued_Tag (Object, I) = Tag);
      pragma Assert (Resolve (Object, 42, Tag) = 0);
      pragma Assert (Resolve_Retired (Object, 42, Tag) = 0);
      Finalize (Object, 43, Tag, True, OK); pragma Assert (not OK);
      Finalize (Object, 42, Tag, I mod 2 = 0, OK); pragma Assert (OK);
      pragma Assert (Resolve (Object, 42, Tag) = (if I mod 2 = 0 then Tag else 0));
      pragma Assert (Resolve (Object, 43, Tag) = 0);
      pragma Assert (Resolve_Retired (Object, 42, Tag) =
        (if I mod 2 = 0 then 0 else Tag));
      pragma Assert (Resolve_Retired (Object, 43, Tag) = 0);
      Finalize (Object, 42, Tag, True, OK); pragma Assert (not OK);
      Old_Tag := Tag;
      Close (Object, 43, Tag);
      pragma Assert (Resolve (Object, 42, Tag) = (if I mod 2 = 0 then Tag else 0));
      Close (Object, 42, Tag); pragma Assert (Resolve (Object, 42, Old_Tag) = 0);
      pragma Assert (Storage_Index (Object, Old_Tag) = I);
      pragma Assert (Issued_Tag (Object, I) = Old_Tag);
      pragma Assert (Resolve_Retired (Object, 42, Old_Tag) = Old_Tag);
      Finalize (Object, 42, Tag, True, OK); pragma Assert (not OK);
   end loop;
   Reserve (Object, 42, Tag); pragma Assert (Tag = 0);
   pragma Assert (Resolve (Object, 42, 42) = 0); -- default PID tag is not a session
   pragma Assert (Resolve (Object, 42, Unsigned_64'Last) = 0);
   pragma Assert (Resolve_Retired (Object, 42, Unsigned_64'Last) = 0);
   pragma Assert (Resolve_Retired (Object, 0, Tag_Base + 1) = 0);
   pragma Assert (Resolve_Retired (Object, 42, Tag_Base) = 0);
   declare Fresh : Registry; begin
      Reserve (Fresh, 42, Tag); Finalize (Fresh, 42, Tag, True, OK);
      pragma Assert (OK and Resolve (Fresh, 42, Tag) = Tag);
      Quarantine (Fresh); pragma Assert (Resolve (Fresh, 42, Tag) = 0);
      pragma Assert (Storage_Index (Fresh, Tag) = 1);
      pragma Assert (Issued_Tag (Fresh, 1) = Tag);
      pragma Assert (Issued_Tag (Fresh, 2) = 0);
      Close (Fresh, 42, Tag);
      pragma Assert (Resolve_Retired (Fresh, 42, Tag) = 0);
      Reserve (Fresh, 42, Tag); pragma Assert (Tag = 0);
   end;
   declare
      Sessions : Registry;
      Buffers : Intel_GPU_Buffer_Handles.Registry;
      Buffer_ID : Intel_GPU_Buffer_Handles.Handle;
      Key : Unsigned_64;
   begin
      Reserve (Sessions, 42, Old_Tag);
      Finalize (Sessions, 42, Old_Tag, True, OK); pragma Assert (OK);
      Key := Resolve (Sessions, 42, Old_Tag);
      Intel_GPU_Buffer_Handles.Register
        (Buffers, Key,
         Intel_GPU_Buffer_Reply.From_Linear
           (16#2000000#, Intel_GPU_Buffer_Backing.CPU_Base, 4096, 16#2000000#),
         Buffer_ID);
      pragma Assert (Buffer_ID /= 0);
      Close (Sessions, 42, Old_Tag);
      Reserve (Sessions, 42, Tag); -- same numeric PID, fresh admission
      Finalize (Sessions, 42, Tag, True, OK); pragma Assert (OK and Tag /= Old_Tag);
      pragma Assert (Resolve_Retired (Sessions, 42, Tag) = 0);
      pragma Assert (Resolve_Retired (Sessions, 42, Old_Tag) = Old_Tag);
      pragma Assert (not Intel_GPU_Buffer_Handles.Resolve
        (Buffers, Resolve (Sessions, 42, Tag), Buffer_ID).Ready);
      pragma Assert (not Intel_GPU_Buffer_Handles.Resolve
        (Buffers, Resolve (Sessions, 42, Old_Tag), Buffer_ID).Ready);
   end;
   Ada.Text_IO.Put_Line ("Render sessions PASS: 32768 five-operation model sequences, reservation, grant acknowledgement, failed grant, PID reuse, wrong sender, retirement, quarantine");
end Render_Sessions_Tests;
