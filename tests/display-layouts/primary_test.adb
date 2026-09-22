with Ada.Text_IO;
with CuBit.Display_Layouts;

procedure Primary_Test is
   use CuBit.Display_Layouts;
   type Order_Array is array (1 .. 4) of Positive;
   Order : Order_Array;
   Ready : Layout;
   Cases : Natural := 0;

   function Present (Mask : Natural; ID : Natural) return Boolean is
     (ID in 1 .. 4 and then (Mask / 2 ** (ID - 1)) mod 2 = 1);
begin
   -- All 24 discovery orders, all readiness subsets, absent/present preferred
   -- and previous identities, both explicit-apply and ordinary hotplug policy.
   for Encoding in 0 .. 255 loop
      declare
         Remaining : Natural := Encoding;
         Distinct : Boolean := True;
      begin
         for I in Order'Range loop
            Order (I) := Remaining mod 4 + 1;
            Remaining := Remaining / 4;
            for J in 1 .. I - 1 loop
               if Order (I) = Order (J) then Distinct := False; end if;
            end loop;
         end loop;
         if Distinct then
            for Mask in 0 .. 15 loop
               Ready := (others => <>);
               for ID of Order loop
                  if Present (Mask, ID) then
                     Ready.Count := Ready.Count + 1;
                     Ready.Items (Ready.Count).Display := Named_Display_ID (ID);
                  end if;
               end loop;
               for Preferred in 1 .. 5 loop
                  for Old in 0 .. 5 loop
                     for Policy in Primary_Update loop
                        declare
                           -- Intentionally stale index: only identity survives
                           -- re-enumeration. Zero encodes no previous choice
                           -- in this test's enumeration, not in the API.
                           Previous : constant Primary_Selection :=
                             (if Old = 0 then (Available => False) else
                                (True, Max_Viewports, Named_Display_ID (Old)));
                           Result : constant Primary_Selection := Select_Primary
                             (Ready, Named_Display_ID (Preferred), Previous,
                              Policy);
                           Expected : Natural := 0;
                        begin
                           if Policy = Preserve_Usable_Primary and then
                             Present (Mask, Old)
                           then
                              Expected := Old;
                           elsif Present (Mask, Preferred) then
                              Expected := Preferred;
                           else
                              -- Independent oracle walks identity order, not
                              -- the model's input array or selection helper.
                              for ID in 1 .. 4 loop
                                 if Present (Mask, ID) then
                                    Expected := ID;
                                    exit;
                                 end if;
                              end loop;
                           end if;
                           pragma Assert (Result.Available = (Expected /= 0));
                           if Result.Available then
                              pragma Assert
                                (Result.Display = Named_Display_ID (Expected));
                              pragma Assert (Result.Index <= Ready.Count);
                              pragma Assert
                                (Ready.Items (Result.Index).Display =
                                   Result.Display);
                           end if;
                           Cases := Cases + 1;
                        end;
                     end loop;
                  end loop;
               end loop;
            end loop;
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("PASS Desktop primary selection cases:" & Cases'Image);
end Primary_Test;
