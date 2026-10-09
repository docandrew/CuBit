with DMA_Record_Blocks;
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Ada.Text_IO;
procedure DMA_Record_Guards_Test is
   type Memory is array (1 .. 4096) of aliased Unsigned_64;
   RAM : Memory with Alignment => 4096;
   Used : Storage_Count := 0;
   function Allocate (Bytes, Alignment : Storage_Count) return Address is
      Result : constant Address := To_Address (To_Integer (RAM'Address) + Integer_Address (Used));
   begin
      if Used + Bytes > RAM'Size / 8 then return Null_Address; end if;
      Used := Used + Bytes;
      return Result;
   end Allocate;
   package R is new DMA_Record_Blocks (Unsigned_64, 16, Allocate);
   use type R.Result;
   use type R.Reference;
   Pool, Other : R.Pool;
   Items : R.List;
   Ref, Alias, Popped : R.Reference;
   Status : R.Result;
   Value : Unsigned_64;
   procedure Check (OK : Boolean) is
   begin
      if not OK then raise Program_Error with "metadata guard regression"; end if;
   end Check;
begin
   -- This unit is deliberately compiled with -gnatp, like the kernel.
   -- A test accidentally enabling assertions must fail here.
   pragma Assert (False);
   R.Reserve (Pool, 42, 32768, Ref, Status);
   Check (Status = R.Ready);
   Alias := Ref;
   R.Push (Items, Ref);
   for Operation in 1 .. 4 loop
      declare Denied : Boolean := False; begin
         begin
            case Operation is
               when 1 => R.Push (Items, Ref);
               when 2 => R.Release (Pool, Ref);
               when 3 => R.Set_Value (Ref, 99);
               when 4 => R.Move (Items, Items);
            end case;
         exception when R.Metadata_Error => Denied := True; end;
         Check (Denied and then R.Value (Alias) = 42);
      end;
   end loop;
   R.Pop (Items, Popped);
   Check (Popped = Alias and R.Empty (Items));
   declare Denied : Boolean := False; begin
      begin R.Release (Other, Popped);
      exception when R.Metadata_Error => Denied := True; end;
      Check (Denied and then R.Value (Popped) = 42);
   end;
   R.Release (Pool, Popped);
   for Operation in 1 .. 3 loop
      declare Denied : Boolean := False; begin
         begin
            case Operation is
               when 1 => R.Release (Pool, Alias);
               when 2 => R.Push (Items, Alias);
               when 3 => Value := R.Value (Alias);
            end case;
         exception when R.Metadata_Error => Denied := True; end;
         Check (Denied);
      end;
   end loop;
   R.Reserve (Pool, 77, 32768, Ref, Status);
   Check (Status = R.Ready and then R.Value (Ref) = 77);
   Ada.Text_IO.Put_Line ("PASS metadata guards with assertions disabled: linked/wrong-pool/double release, double push, self move, released read; pool intact");
end DMA_Record_Guards_Test;
