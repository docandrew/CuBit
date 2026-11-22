with Ada.Text_IO;
with Buddy_Blocks; use Buddy_Blocks;
procedure Main is
   L, R : Descriptor;
   OK : Boolean;
   function Make (State : Block_Kind; Size : Order) return Descriptor is
      Item : Descriptor;
      Accepted : Boolean;
   begin
      if State = Reserved then
         return Item;
      end if;
      Admit (Item, Size, State /= Interior, Accepted);
      pragma Assert (Accepted);
      case State is
         when Listed => Move (Item, Size, Publish, Accepted);
         when Allocated | Retiring =>
            Move (Item, Size, Commit, Accepted);
            pragma Assert (Accepted);
            if State = Retiring then
               Move (Item, Size, Defer, Accepted);
            end if;
         when others => null;
      end case;
      pragma Assert (Accepted);
      return Item;
   end Make;
begin
   pragma Assert (Descriptor'Size = 16 and Descriptor'Object_Size = 16);
   -- Cross every reachable descriptor state/order with every requested
   -- operation/order. Enabled contracts check both success and exact failure
   -- preservation; these calls execute the production core, not a model.
   for State in Block_Kind loop
      for Size in Order loop
         for Requested in Order loop
            for Action in Transition loop
               declare
                  Item : Descriptor := Make (State, Size);
               begin
                  Move (Item, Requested, Action, OK);
                  pragma Assert (OK = (State = Source (Action) and Size = Requested));
               end;
            end loop;
            for Other_State in Block_Kind loop
               for Other_Size in Order loop
                  L := Make (State, Size);
                  R := Make (Other_State, Other_Size);
                  Split (L, R, Requested, OK);
                  L := Make (State, Size);
                  R := Make (Other_State, Other_Size);
                  Merge (L, R, Requested, OK);
               end loop;
            end loop;
         end loop;
      end loop;
   end loop;
   -- Every representable order and operation, including rejected transitions.
   for O in Order loop
      declare
         Item : Descriptor;
      begin
         for A in Transition loop
            Move (Item, O, A, OK);
            pragma Assert (not OK);
         end loop;
         Admit (Item, O, True, OK);
         pragma Assert (OK);
         Move (Item, O, Publish, OK);
         pragma Assert (OK);
         Move (Item, O, Release_Block, OK);
         pragma Assert (not OK);
         Move (Item, O, Remove, OK);
         pragma Assert (OK);
         Move (Item, O, Commit, OK);
         pragma Assert (OK);
         for Wrong in Order loop
            if Wrong /= O then
               Move (Item, Wrong, Release_Block, OK);
               pragma Assert (not OK);
            end if;
         end loop;
         Move (Item, O, Defer, OK);
         pragma Assert (OK);
         Move (Item, O, Release_Block, OK);
         pragma Assert (not OK);
         Move (Item, O, Reclaim, OK);
         pragma Assert (OK);
         Move (Item, O, Reclaim, OK);
         pragma Assert (not OK);
      end;
   end loop;
   L := Make (Reserved, 0);
   R := Make (Reserved, 0);
   Admit (L, 1, True, OK);
   pragma Assert (OK);
   Admit (R, 0, False, OK);
   pragma Assert (OK);
   Split (L, R, 1, OK);
   pragma Assert (OK);
   Merge (L, R, 0, OK);
   pragma Assert (OK);
   Merge (L, R, 1, OK);
   pragma Assert (not OK);
   Ada.Text_IO.Put_Line ("PASS buddy block state transitions");
end Main;
