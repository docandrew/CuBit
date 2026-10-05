with Ada.Text_IO; with Ada.Command_Line;
with System; with System.Storage_Elements; with Interfaces;
with Native_Scene_Bridge;
procedure Native_Scene_Bridge_Tests is
   package B renames Native_Scene_Bridge;
   subtype U32 is Interfaces.Unsigned_32;
   use type U32;
   procedure C_ABI with Import, Convention => C, External_Name => "native_scene_c_abi_test";
   procedure Reset with Import, Convention => C, External_Name => "submission_mock_reset";
   procedure Set (Index, Value : U32) with Import, Convention => C, External_Name => "submission_mock_set";
   function Calls (Index : U32) return U32 with Import, Convention => C, External_Name => "submission_mock_calls";
   procedure Mock (Bytes : Interfaces.Unsigned_64; Mask, Prep, Bind, Release : U32)
     with Import, Convention => C, External_Name => "image_mock";
   function Releases return U32 with Import, Convention => C, External_Name => "image_mock_releases";
   function Addr (N : Natural) return System.Address is
     (System.Storage_Elements.To_Address (System.Storage_Elements.Integer_Address (N)));
   Result, Slot : U32;
   Failure : constant Natural := (if Ada.Command_Line.Argument_Count = 0 then 0
                                  else Natural'Value (Ada.Command_Line.Argument (1)));
   procedure Open is
   begin B.Open (Addr (44), Addr (1), Addr (2), Addr (3), Addr (99), Addr (55), 1, 64, 64, Result); end Open;
   procedure No_Release is
   begin
      B.Release_Frame (Result); pragma Assert (Result /= 0);
      B.Close (Result); pragma Assert (Result /= 0 and Releases = 0);
      B.Begin_Frame (Slot, Result); pragma Assert (Result /= 0 and Slot = 0);
   end No_Release;
begin
   C_ABI;
   Reset; Mock (4096, 1, 0, 0, 0);
   B.Record_Frame (Result); pragma Assert (Result = 2);
   B.Submit_Frame (Result); pragma Assert (Result = 2);
   B.Poll_Frame (Result); pragma Assert (Result = 2);
   B.Cancel_Frame (Result); pragma Assert (Result = 2);
   B.Close (Result); pragma Assert (Result = 2);
   B.Open (Addr (44), Addr (1), Addr (2), Addr (3), Addr (99), Addr (55), 1, U32'Last, 64, Result);
   pragma Assert (Result = 2 and Calls (0) = 0);
   Open; pragma Assert (Result = 0);
   Open; pragma Assert (Result = 2);
   if Failure = 1 then Set (0, 2); end if;
   B.Begin_Frame (Slot, Result);
   if Failure = 1 then pragma Assert (Result = 3 and Slot = 0); No_Release;
   else
      pragma Assert (Result = 0 and Slot in 1 .. 3);
      No_Release;
      B.Submit_Frame (Result); pragma Assert (Result = 2 and Calls (2) = 0);
      if Failure = 2 then Set (7, 2);
      elsif Failure = 3 then Set (6, 1);
      elsif Failure = 4 then Set (8, 2);
      elsif Failure = 5 then Set (6, 1); Set (4, 2);
      end if;
      B.Record_Frame (Result);
      pragma Assert (Calls (2) = 0);
      if Failure in 2 | 4 | 5 then pragma Assert (Result = 3); No_Release;
      elsif Failure = 3 then
         pragma Assert (Result = 2);
         B.Close (Result); pragma Assert (Result = 0 and Releases = 3);
      else
         pragma Assert (Result = 0);
         B.Record_Frame (Result); pragma Assert (Result = 2);
         No_Release;
         if Failure = 6 then Set (1, 2);
         elsif Failure = 7 then Set (2, 2);
         end if;
         B.Submit_Frame (Result);
         if Failure in 6 | 7 then pragma Assert (Result = 3); No_Release;
         else
            pragma Assert (Result = 0);
            B.Cancel_Frame (Result); pragma Assert (Result = 2 and Calls (4) = 0);
            B.Submit_Frame (Result); pragma Assert (Result = 2 and Calls (2) = 1);
            Set (3, 1);
            for I in 1 .. 1000 loop B.Poll_Frame (Result); pragma Assert (Result = 1); end loop;
            No_Release;
            pragma Assert (Calls (2) = 1);
            Set (3, (if Failure = 8 then 2 else 0));
            B.Poll_Frame (Result);
            if Failure = 8 then pragma Assert (Result = 3); No_Release;
            else
               pragma Assert (Result = 0);
               B.Close (Result); pragma Assert (Result = 2 and Releases = 0);
               B.Begin_Frame (Slot, Result); pragma Assert (Result = 2 and Slot = 0);
               B.Release_Frame (Result); pragma Assert (Result = 0);
               B.Release_Frame (Result); pragma Assert (Result = 2);
               B.Close (Result); pragma Assert (Result = 0 and Releases = 3);
            end if;
         end if;
      end if;
   end if;
   if Failure = 0 then
      for I in 1 .. 100 loop
         Reset; Mock (4096, 1, 0, 0, 0);
         Open; pragma Assert (Result = 0);
         B.Begin_Frame (Slot, Result); pragma Assert (Result = 0);
         B.Cancel_Frame (Result); pragma Assert (Result = 0);
         B.Close (Result); pragma Assert (Result = 0 and Releases = 3);
      end loop;
   end if;
   Ada.Text_IO.Put_Line ("NATIVE-SCENE-BRIDGE: PASS scenario" & Natural'Image (Failure));
end Native_Scene_Bridge_Tests;
