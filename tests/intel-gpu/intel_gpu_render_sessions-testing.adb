package body Intel_GPU_Render_Sessions.Testing is
   procedure Run is
      Object : Registry;
      First, Penultimate, Last, Rejected : Unsigned_64;
      Accepted : Boolean;
   begin
      -- Check interleavings, not just one happy-path lifecycle. Keep the
      -- oracle independent of private registry fields: public identity and
      -- authorization results must agree after every prefix, including
      -- quarantine followed by attempted admission and stale finalization.
      declare
         Depth : constant := 5;
         Choices : constant := 8;
         type Model_Phase is (Waiting, Live, Closed);
         type Model_Entry is record
            Owner : Unsigned_64 := 0;
            Phase : Model_Phase := Waiting;
         end record;
         type Model_Array is array (1 .. Depth) of Model_Entry;
      begin
         for Sequence in 0 .. Choices ** Depth - 1 loop
            declare
               Actual : Registry;
               Model : Model_Array;
               Count : Natural := 0;
               Failed : Boolean := False;
               Code : Natural := Sequence;
               Action, Target : Natural;
               Owner, Identity, New_Tag : Unsigned_64;
               Got, Expected : Boolean;
            begin
               for Step in 1 .. Depth loop
                  Action := Code mod Choices;
                  Code := Code / Choices;
                  -- Alternate newest and oldest identity so an operation on
                  -- one session cannot accidentally alter another session.
                  Target := (if Step mod 2 = 0 then Count else 1);
                  Identity := Tag_Base + Unsigned_64 (Target);
                  Owner := (if Action mod 2 = 0 then 42 else 43);
                  case Action is
                     when 0 | 1 =>
                        Reserve (Actual, Owner, New_Tag);
                        if Failed then
                           pragma Assert (New_Tag = 0);
                        else
                           Count := Count + 1;
                           Model (Count) := (Owner, Waiting);
                           pragma Assert (New_Tag = Tag_Base + Unsigned_64 (Count));
                        end if;
                     when 2 .. 5 =>
                        Expected := not Failed and then Target in 1 .. Count
                          and then Model (Target).Owner = Owner
                          and then Model (Target).Phase = Waiting;
                        Finalize (Actual, Owner, Identity, Action < 4, Got);
                        pragma Assert (Got = Expected);
                        if Expected then
                           Model (Target).Phase :=
                             (if Action < 4 then Live else Closed);
                        end if;
                     when 6 =>
                        Close (Actual, Owner, Identity);
                        if not Failed and then Target in 1 .. Count and then
                          Model (Target).Owner = Owner
                        then Model (Target).Phase := Closed; end if;
                     when others =>
                        Quarantine (Actual);
                        Failed := True;
                  end case;
                  for I in 1 .. Depth loop
                     Identity := Tag_Base + Unsigned_64 (I);
                     pragma Assert (Storage_Index (Actual, Identity) =
                       (if I <= Count then I else 0));
                     pragma Assert (Issued_Tag (Actual, I) =
                       (if I <= Count then Identity else 0));
                     for Sender in Unsigned_64 range 41 .. 44 loop
                        Expected := not Failed and then I <= Count and then
                          Model (I).Owner = Sender;
                        pragma Assert (Resolve (Actual, Sender, Identity) =
                          (if Expected and then Model (I).Phase = Live
                           then Identity else 0));
                        pragma Assert (Resolve_Retired (Actual, Sender, Identity) =
                          (if Expected and then Model (I).Phase = Closed
                           then Identity else 0));
                     end loop;
                  end loop;
               end loop;
            end;
         end loop;
      end;
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
