with Interfaces; use Interfaces;
package body Intel_GPU_Display_Lease is
   Current : State_Kind := Idle;
   Owned, Unknown, Added_Requests : Unsigned_64 := 0;
   function Bit (Item : Well) return Unsigned_64 is
     (Shift_Left (Unsigned_64'(1), Well'Pos (Item)));
   function State return State_Kind is (Current);
   function Retained return Unsigned_64 is (Owned or Unknown);
   function Uncertain return Unsigned_64 is (Unknown);
   procedure Acquire (Required : Unsigned_64; Success : out Boolean) is
      Added, OK : Boolean;
      Available : Unsigned_64 := 0;
   begin
      Success := False;
      if Current /= Idle or Well'Pos (Well'Last) >= 64 then return; end if;
      for Item in Well loop Available := Available or Bit (Item); end loop;
      if Required = 0 or else (Required and not Available) /= 0 or else
        not Selection_Valid (Required)
      then return; end if;
      Current := Faulted;
      for Item in Well loop
         if (Required and Bit (Item)) /= 0 then
            Unknown := Unknown or Bit (Item);
            Hold_Well (Item, Added, OK);
            if not OK then return; end if;
            Owned := Owned or Bit (Item);
            Unknown := Unknown and not Bit (Item);
            if Added then Added_Requests := Added_Requests or Bit (Item); end if;
         end if;
      end loop;
      Current := Held;
      Success := True;
   end Acquire;
   procedure Release (Success : out Boolean) is
      OK : Boolean;
   begin
      Success := False;
      if Current /= Held then return; end if;
      Current := Faulted;
      for Item in reverse Well loop
         if (Owned and Bit (Item)) /= 0 then
            Unknown := Unknown or Bit (Item);
            Drop_Well (Item, (Added_Requests and Bit (Item)) /= 0, OK);
            -- Do not power down any ancestor after a child release failure.
            if not OK then return; end if;
            Unknown := Unknown and not Bit (Item);
            Added_Requests := Added_Requests and not Bit (Item);
         end if;
         Owned := Owned and not Bit (Item);
      end loop;
      Current := Idle;
      Success := True;
   end Release;
end Intel_GPU_Display_Lease;
