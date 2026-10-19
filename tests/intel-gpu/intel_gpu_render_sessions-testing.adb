package body Intel_GPU_Render_Sessions.Testing is
   type Registry_Access is access Registry;
   procedure Run is
      Object : Registry renames Registry_Access'(new Registry).all;
      First, Penultimate, Last, Rejected : Unsigned_64;
      Accepted : Boolean;
      Released : constant Release_Facts := (others => True);
   begin
      -- Check interleavings, not just one happy-path lifecycle. Keep the
      -- oracle independent of private registry fields: public identity and
      -- authorization results must agree after every prefix, including
      -- quarantine followed by attempted admission, stale finalization and
      -- recycling.
      declare
         Depth : constant := 5;
         Choices : constant := 9;
         Slots : constant := 3;
         type Model_Phase is (Free, Waiting, Live, Closed);
         type Model_Entry is record
            Owner : Unsigned_64 := 0;
            Tag : Unsigned_64 := 0;
            Phase : Model_Phase := Free;
         end record;
         type Model_Array is array (1 .. Slots) of Model_Entry;
         Actual : Registry renames Registry_Access'(new Registry).all;
      begin
         for Sequence in 0 .. Choices ** Depth - 1 loop
            -- A fresh registry each time (test-only reset of private fields).
            Actual.Grown := 0; Actual.Issued := 0; Actual.Failed := False;
            Actual.Items (1 .. Slots) := [others => (others => <>)];
            declare
               Model : Model_Array;
               Issued : Unsigned_64 := 0;
               Failed : Boolean := False;
               Code : Natural := Sequence;
               Action, Target : Natural;
               Owner, New_Tag : Unsigned_64;
               Got, Expected : Boolean;
               function Lowest_Free return Natural is
               begin
                  for I in Model'Range loop
                     if Model (I).Phase = Free then return I; end if;
                  end loop;
                  return 0;
               end Lowest_Free;
            begin
               Grow (Actual, Slots);
               for Step in 1 .. Depth loop
                  Action := Code mod Choices;
                  Code := Code / Choices;
                  Target := (if Step mod 2 = 0 then Slots else 1);
                  Owner := (if Action mod 2 = 0 then 42 else 43);
                  case Action is
                     when 0 | 1 =>
                        Reserve (Actual, Owner, New_Tag);
                        if Failed or else Lowest_Free = 0 then
                           pragma Assert (New_Tag = 0);
                        else
                           Issued := Issued + 1;
                           Model (Lowest_Free) :=
                             (Owner, Tag_Base + Issued * Slot_Span + Unsigned_64 (Lowest_Free), Waiting);
                           pragma Assert (New_Tag = Model (Storage_Index (Actual, New_Tag)).Tag);
                        end if;
                     when 2 .. 5 =>
                        Expected := not Failed and then Model (Target).Owner = Owner
                          and then Model (Target).Phase = Waiting;
                        Finalize (Actual, Owner, Model (Target).Tag, Action < 4, Got);
                        pragma Assert (Got = Expected);
                        if Expected then
                           Model (Target).Phase := (if Action < 4 then Live else Closed);
                        end if;
                     when 6 =>
                        Close (Actual, Owner, Model (Target).Tag);
                        if not Failed and then Model (Target).Phase /= Free and then
                          Model (Target).Owner = Owner
                        then Model (Target).Phase := Closed; end if;
                     when 7 =>
                        Recycle (Actual, Target, Released, Got);
                        pragma Assert (Got = (not Failed and Model (Target).Phase = Closed));
                        if Got then Model (Target) := (others => <>); end if;
                     when others =>
                        Quarantine (Actual);
                        Failed := True;
                  end case;
                  for I in Model'Range loop
                     pragma Assert (Issued_Tag (Actual, I) = Model (I).Tag);
                     if Model (I).Tag /= 0 then
                        pragma Assert (Storage_Index (Actual, Model (I).Tag) = I);
                     end if;
                     pragma Assert (Retired (Actual, I) = (Model (I).Phase = Closed));
                     for Sender in Unsigned_64 range 41 .. 44 loop
                        Expected := not Failed and then Model (I).Phase /= Free and then
                          Model (I).Owner = Sender;
                        pragma Assert (Resolve (Actual, Sender, Model (I).Tag) =
                          (if Expected and then Model (I).Phase = Live
                           then Model (I).Tag else 0));
                        pragma Assert (Resolve_Retired (Actual, Sender, Model (I).Tag) =
                          (if Expected and then Model (I).Phase = Closed
                           then Model (I).Tag else 0));
                     end loop;
                  end loop;
               end loop;
            end;
         end loop;
      end;
      -- Tag exhaustion: skip the uninteresting issue range (test-only child,
      -- no production reset/seed API). Existing records keep their tags.
      Grow (Object, 4);
      Reserve (Object, 42, First);
      Finalize (Object, 42, First, True, Accepted);
      pragma Assert (Accepted);
      Object.Issued := Issue_Limit - 2;
      Reserve (Object, 42, Penultimate);
      Reserve (Object, 43, Last);
      pragma Assert (Penultimate = Tag_Base + (Issue_Limit - 1) * Slot_Span + 2);
      pragma Assert (Last = Tag_Base + Issue_Limit * Slot_Span + 3 and Last <= Tag_Last);
      pragma Assert (Storage_Index (Object, First) = 1);
      pragma Assert (Storage_Index (Object, Penultimate) = 2);
      pragma Assert (Storage_Index (Object, Last) = 3);
      pragma Assert (Exhausted (Object));
      Finalize (Object, 42, Penultimate, True, Accepted);
      pragma Assert (Accepted and Resolve (Object, 42, Penultimate) = Penultimate);
      Finalize (Object, 43, Last, True, Accepted);
      pragma Assert (Accepted and Resolve (Object, 43, Last) = Last);
      for Attempt in 1 .. 32 loop
         Reserve (Object, 44, Rejected);
         pragma Assert (Rejected = 0 and Object.Issued = Issue_Limit);
         pragma Assert (Resolve (Object, 42, First) = First);
         pragma Assert (Resolve (Object, 43, Last) = Last);
      end loop;
      Close (Object, 43, Last);
      Recycle (Object, 3, Released, Accepted);
      pragma Assert (Accepted);
      Reserve (Object, 44, Rejected);
      pragma Assert (Rejected = 0); -- a free slot, but no tag left to issue
      Quarantine (Object);
      pragma Assert (Resolve (Object, 42, First) = 0);
      pragma Assert (Issued_Tag (Object, 2) = Penultimate);
   end Run;
end Intel_GPU_Render_Sessions.Testing;
