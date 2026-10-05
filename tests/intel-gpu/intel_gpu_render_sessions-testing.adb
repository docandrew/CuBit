package body Intel_GPU_Render_Sessions.Testing is
   procedure Run is
      Object : Registry;
      First, Penultimate, Last, Rejected : Unsigned_64;
      Accepted : Boolean;
   begin
      Reserve (Object, 42, First);
      Finalize (Object, 42, First, True, Accepted);
      pragma Assert (Accepted);
      -- Skip the uninteresting issuance range without adding a production
      -- reset/seed API. Existing records retain their original identities.
      Object.Last_Issued := Tag_Last - 2;
      Reserve (Object, 42, Penultimate);
      Reserve (Object, 43, Last);
      pragma Assert (Penultimate = Tag_Last - 1 and Last = Tag_Last);
      pragma Assert (Storage_Index (Object, First) = 1);
      pragma Assert (Storage_Index (Object, Penultimate) = 2);
      pragma Assert (Storage_Index (Object, Last) = 3);
      pragma Assert (Issued_Tag (Object, 2) = Penultimate);
      pragma Assert (Issued_Tag (Object, 3) = Last);
      pragma Assert (Storage_Index (Object, Tag_Base + 2) = 0);
      Finalize (Object, 42, Penultimate, True, Accepted);
      pragma Assert (Accepted and Resolve (Object, 42, Penultimate) = Penultimate);
      Finalize (Object, 43, Last, True, Accepted);
      pragma Assert (Accepted and Resolve (Object, 43, Last) = Last);
      for Attempt in 1 .. 32 loop
         Reserve (Object, 44, Rejected);
         pragma Assert (Rejected = 0 and Object.Last_Issued = Tag_Last);
         pragma Assert (Object.Used = 3);
         pragma Assert (Resolve (Object, 42, First) = First);
         pragma Assert (Resolve (Object, 43, Last) = Last);
      end loop;
      Close (Object, 43, Last);
      Reserve (Object, 44, Rejected);
      pragma Assert (Rejected = 0 and Resolve_Retired (Object, 43, Last) = Last);
      Quarantine (Object);
      pragma Assert (Resolve (Object, 42, First) = 0);
      pragma Assert (Issued_Tag (Object, 3) = Last);
      Reserve (Object, 44, Rejected);
      pragma Assert (Rejected = 0 and Object.Last_Issued = Tag_Last);
   end Run;
end Intel_GPU_Render_Sessions.Testing;
