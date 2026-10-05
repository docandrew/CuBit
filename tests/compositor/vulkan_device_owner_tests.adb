with Ada.Text_IO;
with Interfaces;
with System;
with System.Storage_Elements;
with Vulkan_Device_Owner;
with Vulkan_Device_Mock;
with Vulkan_Context_Owner;
with Vulkan_Submission;
procedure Vulkan_Device_Owner_Tests is
   package D renames Vulkan_Device_Owner;
   package M renames Vulkan_Device_Mock;
   package C renames Vulkan_Context_Owner;
   package V renames Vulkan_Submission;
   use type D.Phase, C.Phase, C.Child, Interfaces.Unsigned_32, V.Observation, System.Address;
   procedure Context_Set (Creation, Retirement : Interfaces.Unsigned_32)
     with Import, Convention => C, External_Name => "context_mock_set";
   procedure Submission_Reset with Import, Convention => C, External_Name => "submission_mock_reset";
   procedure Submission_Result (Index, Value : Interfaces.Unsigned_32)
     with Import, Convention => C, External_Name => "submission_mock_set";
begin
   for Owned in Boolean loop
      for Description in Boolean loop
         for Creation in 0 .. 2 loop
            for Retirement in 0 .. 2 loop
               declare
                  S : D.State;
                  Context : C.State;
                  Submission : V.State := V.Open (System.Null_Address);
                  Child : C.Child;
               begin
                  M.Set (Owned, Description, Interfaces.Unsigned_32 (Retirement));
                  Context_Set (Interfaces.Unsigned_32 (Creation), 0);
                  D.Start (S, Context, Submission, 25);
                  pragma Assert (M.Starts = 1);
                  D.Start (S, Context, Submission, 25);
                  pragma Assert (M.Starts = 1);
                  if Owned and Description and Creation = 0 then
                     pragma Assert (D.Current (S) = D.Ready);
                     C.Register_Child (Context, Child);
                     pragma Assert (Child /= C.No_Child);
                     D.Close (S, Context, Submission);
                     pragma Assert (M.Closes = 0 and C.Held (Context, Child));
                     C.Retire_Child (Context, Child, False);
                     D.Close (S, Context, Submission);
                     pragma Assert (M.Closes = 0);
                     C.Retire_Child (Context, Child, True);
                  end if;
                  D.Close (S, Context, Submission);
                  if (not Owned and Description) or
                    (Owned and Description and Creation = 2)
                  then
                     pragma Assert (D.Current (S) = D.Quarantined and M.Closes = 0);
                  elsif not Owned then
                     pragma Assert (D.Current (S) = D.Retired and M.Closes = 0);
                  else
                     pragma Assert (M.Closes = 1);
                     case Retirement is
                        when 0 => pragma Assert (D.Current (S) = D.Retired and not D.Owns_Device (S));
                        when 1 => pragma Assert (D.Current (S) = D.Retiring and D.Owns_Device (S));
                        when others => pragma Assert (D.Current (S) = D.Quarantined and D.Owns_Device (S));
                     end case;
                     D.Close (S, Context, Submission);
                     pragma Assert (M.Closes = (if Retirement = 1 then 2 else 1));
                  end if;
               end;
            end loop;
         end loop;
      end loop;
   end loop;
   declare
      S : D.State;
      Context : C.State;
      Submission : V.State := V.Open (System.Null_Address);
   begin
      M.Set (True, True, 0);
      D.Start (S, Context, Submission, 0);
      pragma Assert (D.Current (S) = D.Software and M.Starts = 0);
      D.Start (S, Context, Submission, 25);
      pragma Assert (M.Starts = 0);
      D.Close (S, Context, Submission);
      pragma Assert (D.Current (S) = D.Retired and M.Closes = 0);
   end;
   for Release_Code in 0 .. 2 loop
      declare
         S : D.State;
         Context, Foreign : C.State;
         Submission : V.State := V.Open (System.Null_Address);
         Source : V.Source_Ticket;
         Address : System.Address;
         OK : Boolean;
         Outcome : V.Observation;
         use System.Storage_Elements;
      begin
         M.Set (True, True, 0); Context_Set (0, Interfaces.Unsigned_32 (Release_Code));
         Submission_Reset;
         D.Start (S, Context, Submission, 25);
         -- A foreign/fresh context or a foreign idle submission cannot retire
         -- the process device, even when both have no outstanding children.
         D.Close (S, Foreign, Submission); pragma Assert (M.Closes = 0);
         D.Close (S, Context, V.Open (To_Address (512))); pragma Assert (M.Closes = 0);
         V.Install_Source (Submission, 0, To_Address (1024), Source);
         D.Close (S, Context, Submission); pragma Assert (M.Closes = 0);
         V.Remove_Source (Submission, Source, Address); pragma Assert (Address /= System.Null_Address);
         V.Begin_Record (Submission, OK); pragma Assert (OK);
         D.Close (S, Context, Submission); pragma Assert (M.Closes = 0);
         V.Begin_Scene (Submission, To_Address (2048), 64, 64, OK); pragma Assert (OK);
         V.End_Scene (Submission, OK); pragma Assert (OK);
         V.Seal (Submission, OK); pragma Assert (OK);
         V.Submit (Submission, OK); pragma Assert (OK);
         Submission_Result (3, 1); V.Poll (Submission, Outcome);
         pragma Assert (Outcome = V.Still_Pending);
         D.Close (S, Context, Submission); pragma Assert (M.Closes = 0);
         Submission_Result (3, 0); V.Poll (Submission, Outcome);
         pragma Assert (Outcome = V.Finished);
         D.Close (S, Context, Submission);
         if Release_Code = 0 then
            pragma Assert (D.Current (S) = D.Retired and M.Closes = 1);
         else
            pragma Assert (D.Current (S) = D.Quarantined and M.Closes = 0);
            D.Close (S, Context, Submission); pragma Assert (M.Closes = 0);
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("PASS device startup/child retirement/one-attempt/authority retention/pending GPU");
end Vulkan_Device_Owner_Tests;
