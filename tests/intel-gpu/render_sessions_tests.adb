with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Render_Sessions; use Intel_GPU_Render_Sessions;
with Intel_GPU_Render_Sessions.Testing;
with Intel_GPU_Buffer_Handles;
with Intel_GPU_Buffer_Backing;
with Intel_GPU_Buffer_Reply;
procedure Render_Sessions_Tests is
   -- A registry has a slot for every GuC context ID: keep it off the stack.
   type Registry_Access is access Registry;
   Object : Registry renames Registry_Access'(new Registry).all;
   Tag, Old_Tag : Unsigned_64;
   OK : Boolean;
   Released : constant Release_Facts := (others => True);
   Count : constant := 32;
begin
   Intel_GPU_Render_Sessions.Testing.Run;
   pragma Assert (Grown (Object) = 0);
   pragma Assert (Issued_Tag (Object, 0) = 0);
   pragma Assert (Storage_Index (Object, Tag_Base + Slot_Span + 1) = 0);
   pragma Assert (Storage_Index (Object, 0) = 0);
   pragma Assert (Storage_Index (Object, Unsigned_64'Last) = 0);
   -- No storage, no session: the caller grows first.
   Reserve (Object, 42, Tag); pragma Assert (Tag = 0);
   Grow (Object, Count);
   for I in 1 .. Count loop
      pragma Assert (Issued_Tag (Object, I) = 0);
   end loop;
   Reserve (Object, 0, Tag); pragma Assert (Tag = 0);
   for I in 1 .. Count loop
      Reserve (Object, 42, Tag);
      pragma Assert (Tag = Tag_Base + Unsigned_64 (I) * Slot_Span + Unsigned_64 (I));
      pragma Assert (Storage_Index (Object, Tag) = I and Tag_Slot (Tag) = I);
      pragma Assert (Issued_Tag (Object, I) = Tag and Sender_Of (Object, I) = 42);
      pragma Assert (Was_Issued (Object, Tag) and Holds_Nothing (Object, I));
      pragma Assert (Context_ID (I) = 7 + Unsigned_32 (I));
      pragma Assert (Resolve (Object, 42, Tag) = 0);
      pragma Assert (Resolve_Retired (Object, 42, Tag) = 0);
      Finalize (Object, 43, Tag, True, OK); pragma Assert (not OK);
      Finalize (Object, 42, Tag, I mod 2 = 0, OK); pragma Assert (OK);
      pragma Assert (Resolve (Object, 42, Tag) = (if I mod 2 = 0 then Tag else 0));
      pragma Assert (Resolve (Object, 43, Tag) = 0);
      pragma Assert (Resolve_Retired (Object, 42, Tag) =
        (if I mod 2 = 0 then 0 else Tag));
      pragma Assert (Retired (Object, I) = (I mod 2 /= 0));
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
   -- Every grown slot holds a retired session: none is free until recycled.
   Reserve (Object, 42, Tag); pragma Assert (Tag = 0);
   pragma Assert (Resolve (Object, 42, 42) = 0); -- default PID tag is not a session
   pragma Assert (Resolve (Object, 42, Unsigned_64'Last) = 0);
   pragma Assert (Resolve_Retired (Object, 42, Unsigned_64'Last) = 0);
   pragma Assert (Resolve_Retired (Object, 0, Tag_Base + Slot_Span + 1) = 0);
   pragma Assert (Resolve_Retired (Object, 42, Tag_Base) = 0);
   -- Holdings block recycling until given back; the GuC context hold only
   -- by an acknowledged deregistration.
   Old_Tag := Issued_Tag (Object, 5);
   Hold (Object, 5, GuC_Context); Hold (Object, 5, Context_Image);
   pragma Assert (Holds (Object, 5, GuC_Context) and Holds (Object, 5, Context_Image));
   Recycle (Object, 5, Released, OK); pragma Assert (not OK);
   Release_Image (Object, 5);
   pragma Assert (not Holds (Object, 5, Context_Image) and Holds (Object, 5, GuC_Context));
   Recycle (Object, 5, Released, OK); pragma Assert (not OK);
   Acknowledge_Deregistration (Object, 5);
   for Missing in 1 .. 4 loop
      Recycle (Object, 5,
        (Backing_Released => Missing /= 1, Grants_Returned => Missing /= 2,
         Queue_Forgotten => Missing /= 3, Unreferenced => Missing /= 4), OK);
      pragma Assert (not OK and Issued_Tag (Object, 5) = Old_Tag);
   end loop;
   Recycle (Object, 5, Released, OK);
   pragma Assert (OK and Issued_Tag (Object, 5) = 0 and Storage_Index (Object, Old_Tag) = 0);
   pragma Assert (Was_Issued (Object, Old_Tag) and Resolve_Retired (Object, 42, Old_Tag) = 0);
   Recycle (Object, 5, Released, OK); pragma Assert (not OK); -- already free
   -- The freed slot is the one reserved next, under a fresh tag.
   Reserve (Object, 44, Tag);
   pragma Assert (Tag /= 0 and Tag > Old_Tag and Storage_Index (Object, Tag) = 5);
   pragma Assert (Holds_Nothing (Object, 5) and not Retired (Object, 5));
   -- An active session is not recycled.
   Finalize (Object, 44, Tag, True, OK);
   Recycle (Object, 5, Released, OK); pragma Assert (not OK);
   -- Free slots take no holdings.
   Hold (Object, Count, GuC_Context);
   declare Fresh : Registry renames Registry_Access'(new Registry).all; begin
      Grow (Fresh, 2);
      Reserve (Fresh, 42, Tag); Finalize (Fresh, 42, Tag, True, OK);
      pragma Assert (OK and Resolve (Fresh, 42, Tag) = Tag);
      Hold (Fresh, 2, GuC_Context);
      pragma Assert (not Holds (Fresh, 2, GuC_Context));
      Quarantine (Fresh); pragma Assert (Resolve (Fresh, 42, Tag) = 0);
      pragma Assert (Storage_Index (Fresh, Tag) = 1);
      pragma Assert (Issued_Tag (Fresh, 1) = Tag);
      pragma Assert (Issued_Tag (Fresh, 2) = 0);
      Close (Fresh, 42, Tag);
      pragma Assert (Resolve_Retired (Fresh, 42, Tag) = 0);
      Reserve (Fresh, 42, Tag); pragma Assert (Tag = 0);
      -- Nothing is recycled while quarantined.
      Recycle (Fresh, 1, Released, OK); pragma Assert (not OK);
   end;
   declare
      Sessions : Registry renames Registry_Access'(new Registry).all;
      Buffers : Intel_GPU_Buffer_Handles.Registry;
      Buffer_ID : Intel_GPU_Buffer_Handles.Handle;
      Key : Unsigned_64;
   begin
      Grow (Sessions, 1);
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
      Recycle (Sessions, 1, Released, OK); pragma Assert (OK);
      Reserve (Sessions, 42, Tag); -- same numeric PID, same slot, fresh admission
      Finalize (Sessions, 42, Tag, True, OK); pragma Assert (OK and Tag /= Old_Tag);
      pragma Assert (Storage_Index (Sessions, Tag) = Storage_Index (Sessions, Tag));
      pragma Assert (Resolve (Sessions, 42, Old_Tag) = 0);
      pragma Assert (Resolve_Retired (Sessions, 42, Old_Tag) = 0);
      pragma Assert (not Intel_GPU_Buffer_Handles.Resolve
        (Buffers, Resolve (Sessions, 42, Tag), Buffer_ID).Ready);
      pragma Assert (not Intel_GPU_Buffer_Handles.Resolve
        (Buffers, Resolve (Sessions, 42, Old_Tag), Buffer_ID).Ready);
   end;
   Ada.Text_IO.Put_Line ("Render sessions PASS: model sequences, growth, reservation, grant acknowledgement, failed grant, holdings, recycling, PID and slot reuse, wrong sender, retirement, quarantine, tag exhaustion");
end Render_Sessions_Tests;
