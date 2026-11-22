with Ada.Text_IO;
with AML_Frame_Handles; use AML_Frame_Handles;
with AML_Frame_Roots;
with AML_Root_Slots;
with AML_Identity.Issuer;
procedure Held_Pool_Tests is
   package Pool is new AML_Frame_Roots (Natural, 0, 2);
   use type Pool.State;
   use type Pool.Result_Status;
   S : Pool.State := Pool.Empty;
   Status : Pool.Result_Status;
   A, B : AML_Identity.Identity;
   First, Other, Newer : Frame_Handle;
   OK : Boolean;
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Checks'Image; end if;
   end Check;
begin
   AML_Identity.Issuer.Issue (A, OK); Check (OK);
   AML_Identity.Issuer.Issue (B, OK); Check (OK);
   First := Bind_Frame (Bind_Domain (A, 1), 1, 1);
   Other := Bind_Frame (Bind_Domain (B, 1), 1, 1);
   Newer := Bind_Frame (Bind_Domain (A, 2), 1, 1);
   Pool.Reserve (S, No_Frame, Status); Check (Status = Pool.Invalid_Frame and then S = Pool.Empty);
   Pool.Reserve (S, First, Status); Check (Status = Pool.Ready);
   Pool.Reserve (S, Other, Status); Check (Status = Pool.Ready and then Pool.Count (S) = 2);
   declare Before : constant Pool.State := S; begin
      Pool.Reserve (S, First, Status); Check (Status = Pool.Duplicate_Frame and then S = Before);
      Pool.Reserve (S, Newer, Status); Check (Status = Pool.Root_Limit and then S = Before);
      Pool.Update (S, Newer, Local_0, True, 9, Status); Check (Status = Pool.Invalid_Frame and then S = Before);
      Pool.Update (S, First, Local_0, False, 9, Status); Check (Status = Pool.Invalid_Value and then S = Before);
   end;
   for C in Cell_ID loop
      Pool.Update (S, First, C, True, Cell_ID'Pos (C), Status); Check (Status = Pool.Ready);
      Pool.Update (S, Other, C, True, Cell_ID'Pos (C) + 100, Status); Check (Status = Pool.Ready);
   end loop;
   for C in Cell_ID loop
      declare L : constant Pool.Read_Result := Pool.Read_Cell (S, 1, C);
              R : constant Pool.Read_Result := Pool.Read_Cell (S, 2, C); begin
         Check (L.Reserved and then L.Initialized and then L.Value = Cell_ID'Pos (C));
         Check (R.Reserved and then R.Initialized and then R.Value = Cell_ID'Pos (C) + 100);
      end;
   end loop;
   for Root in AML_Root_Slots.Held_Root loop
      Pool.Update_Held (S, First, Root, True, AML_Root_Slots.Held_Root'Pos (Root), Status);
      Check (Status = Pool.Ready);
      declare L : constant Pool.Read_Result := Pool.Read_Held (S, 1, Root);
              R : constant Pool.Read_Result := Pool.Read_Held (S, 2, Root); begin
         Check (L.Reserved and then L.Initialized and then L.Value = AML_Root_Slots.Held_Root'Pos (Root));
         Check (R.Reserved and then not R.Initialized and then R.Value = 0);
      end;
      declare Before : constant Pool.State := S; begin
         Pool.Update_Held (S, Newer, Root, True, 7, Status);
         Check (Status = Pool.Invalid_Frame and then S = Before);
         Pool.Update_Held (S, First, Root, False, 7, Status);
         Check (Status = Pool.Invalid_Value and then S = Before);
      end;
   end loop;
   Pool.Release (S, First, Status); Check (Status = Pool.Ready and then Pool.Count (S) = 1);
   Pool.Reserve (S, Newer, Status); Check (Status = Pool.Ready);
   declare Before : constant Pool.State := S; begin
      Pool.Release (S, First, Status); Check (Status = Pool.Invalid_Frame and then S = Before);
      Pool.Update (S, First, Arg_0, True, 7, Status); Check (Status = Pool.Invalid_Frame and then S = Before);
   end;
   for C in Cell_ID loop
      declare L : constant Pool.Read_Result := Pool.Read_Cell (S, 1, C);
              R : constant Pool.Read_Result := Pool.Read_Cell (S, 2, C); begin
         Check (L.Reserved and then not L.Initialized and then L.Value = 0);
         Check (R.Initialized and then R.Value = Cell_ID'Pos (C) + 100);
      end;
   end loop;
   for Root in AML_Root_Slots.Held_Root loop
      declare Item : constant Pool.Read_Result := Pool.Read_Held (S, 1, Root); begin
         Check (Item.Reserved and then not Item.Initialized and then Item.Value = 0);
      end;
      Pool.Update_Held (S, Newer, Root, True, 9, Status); Check (Status = Pool.Ready);
      Pool.Update_Held (S, Newer, Root, False, 0, Status); Check (Status = Pool.Ready);
   end loop;
   Pool.Release (S, Newer, Status); Check (Status = Pool.Ready);
   Pool.Release (S, Other, Status); Check (Status = Pool.Ready and then S = Pool.Empty);
   Ada.Text_IO.Put_Line ("HELD ROOT POOL" & Checks'Image);
end Held_Pool_Tests;
