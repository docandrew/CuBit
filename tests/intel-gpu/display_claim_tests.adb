with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Display_Claim; use Intel_GPU_Display_Claim;
with Intel_GPU_Display_Pages;
with Intel_GPU_Reset_Pages;
with CuBit.Log_Protocol;
with Intel_GPU_Parent_Writes;
with Intel_GPU_Display_Topology; use Intel_GPU_Display_Topology;
procedure Display_Claim_Tests is
   Allowed : Boolean;
begin
   pragma Assert (Intel_GPU_Display_Pages.Request_Label = 16#0231#);
   pragma Assert (Intel_GPU_Display_Pages.Offset (0) = 16#45000#);
   pragma Assert (Intel_GPU_Display_Pages.Offset (1) = 16#46000#);
   pragma Assert (Intel_GPU_Display_Pages.Offset (2) = 16#44000#);
   for Page in Intel_GPU_Display_Pages.Page_Index loop
      pragma Assert (Intel_GPU_Display_Pages.Slot (Page) =
        (if Page = 2 then 28 else 24 + Unsigned_64 (Page)));
      pragma Assert (Intel_GPU_Display_Pages.Slot (Page) /= 26);
      for Other in Intel_GPU_Display_Pages.Page_Index loop
         if Other /= Page then
            pragma Assert (Intel_GPU_Display_Pages.Slot (Page) /= Intel_GPU_Display_Pages.Slot (Other));
            pragma Assert (Intel_GPU_Display_Pages.Offset (Page) /= Intel_GPU_Display_Pages.Offset (Other));
         end if;
      end loop;
      pragma Assert (Intel_GPU_Display_Pages.Slot (Page) /= CuBit.Log_Protocol.Publisher_Slot);
      pragma Assert (Intel_GPU_Display_Pages.Slot (Page) /= CuBit.Log_Protocol.Observer_Slot);
      for Reset_Page in Intel_GPU_Reset_Pages.Page_Index loop
         pragma Assert (Intel_GPU_Display_Pages.Slot (Page) /= Intel_GPU_Reset_Pages.Slot (Reset_Page));
         pragma Assert (Intel_GPU_Display_Pages.Offset (Page) /= Intel_GPU_Reset_Pages.Offset (Reset_Page));
      end loop;
   end loop;
   pragma Assert (Intel_GPU_Display_Pages.Slot (0) /= Intel_GPU_Display_Pages.Slot (1));
   for Designated in Unsigned_64 range 0 .. 2 loop
      for Caller in Unsigned_64 range 0 .. 2 loop
         for Badge in Boolean loop
            for Device in Boolean loop
               declare
                  Object : Claim;
                  Expected : constant Boolean :=
                    Designated /= 0 and Caller = Designated and Badge and Device;
               begin
                  Take (Object, Designated, Caller, Badge, Device, Allowed);
                  pragma Assert (Allowed = Expected);
                  pragma Assert (Owner (Object) = (if Expected then Caller else 0));
                  if not Expected then
                     Take (Object, 7, 7, True, True, Allowed);
                     pragma Assert (Allowed and Owner (Object) = 7);
                  end if;
                  -- Lost reply, caller retry, and a newly designated process
                  -- all leave the consumed owner unchanged. No rebind API.
                  declare
                     Saved : constant Unsigned_64 := Owner (Object);
                  begin
                     for New_Caller in Unsigned_64 range 0 .. 16 loop
                        Take (Object, New_Caller, New_Caller, True, True, Allowed);
                        pragma Assert (not Allowed and Owner (Object) = Saved);
                     end loop;
                  end;
               end;
            end loop;
         end loop;
      end loop;
   end loop;
   -- Every aligned offset in the admitted register BAR: only the two
   -- documented parent-power registers can accept an otherwise valid write.
   for Word in Unsigned_32 range 0 .. 16#7FFFF# loop
      declare
         Address : constant Unsigned_32 := Word * 4;
      begin
         for Item in Request_Well loop
            pragma Assert (Intel_GPU_Parent_Writes.Allowed (Item, Address, 0, 2) =
              (Item = PW1 and Address = 16#45404#));
            pragma Assert (Intel_GPU_Parent_Writes.Allowed (Item, Address, 0, 16#8000#) =
              (Item = PW1 and Address = 16#46430#));
            pragma Assert (Intel_GPU_Parent_Writes.Allowed (Item, Address, 0, 8) =
              (Item = PW2 and Address = 16#45404#));
         end loop;
      end;
   end loop;
   for Register_Index in 0 .. 2 loop
      declare
         Address : constant Unsigned_32 :=
           (if Register_Index = 1 then 16#46430# else 16#45404#);
         Item : constant Request_Well := (if Register_Index = 2 then PW2 else PW1);
         Mask : constant Unsigned_32 :=
           (case Register_Index is when 0 => 2, when 1 => 16#8000#, when others => 8);
         Samples : constant array (1 .. 3) of Unsigned_32 :=
           [0, 16#55555555#, 16#AAAAAAAA#];
      begin
         for Prior of Samples loop
            pragma Assert (Intel_GPU_Parent_Writes.Allowed (Item, Address, Prior, Prior or Mask));
            for Bit in 0 .. 31 loop
               pragma Assert (not Intel_GPU_Parent_Writes.Allowed
                 (Item, Address, Prior, (Prior or Mask) xor Shift_Left (1, Bit)));
            end loop;
            pragma Assert (not Intel_GPU_Parent_Writes.Allowed
              (Item, Address + 1, Prior, Prior or Mask));
            for Other in Request_Well loop
               if Other not in PW1 | PW2 then
                  pragma Assert (not Intel_GPU_Parent_Writes.Allowed
                    (Other, Address, Prior, Prior or Mask));
               end if;
            end loop;
         end loop;
         pragma Assert (not Intel_GPU_Parent_Writes.Allowed
           (Item, Address, Unsigned_32'Last, Unsigned_32'Last));
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Parent guard PASS: all request wells at every aligned BAR offset, bit preservation, unaligned offsets and sentinel rejection");
   Ada.Text_IO.Put_Line ("Display claim PASS: 36 admission combinations and 612 retry/rebind rejections");
end Display_Claim_Tests;
