with Vulkan_Upload_FFI; with Interfaces;
package body Vulkan_Upload_Owner with SPARK_Mode is
   package F renames Vulkan_Upload_FFI;
   use type F.U32, F.U64;
   procedure Retire (S : in out State; Context : in out C.State)
     with Post => S.Mode = Closed and S.Parent = C.No_Child and S.Ticket = S.Ticket'Old and
       not Parent_Held (S, Context) and
       C.Current (Context) = C.Current (Context'Old) and C.Context (Context) = C.Context (Context'Old)
   is
   begin
      S.Mode := Closed; S.Mapped := System.Null_Address;
      C.Retire_Child (Context, S.Parent, True); S.Parent := C.No_Child;
   end Retire;
   procedure Discard_Unbound (S : in out State; Context : in out C.State)
     with Pre => Parent_Held (S, Context),
       Post => S.Mode in Closed | Quarantined and
         (if S.Mode = Quarantined then Parent_Held (S, Context)) and
         C.Current (Context) = C.Current (Context'Old) and C.Context (Context) = C.Context (Context'Old)
   is
      Result : F.U32;
   begin
      F.Release (S.Request, Result);
      if Result = 0 then Retire (S, Context); else S.Mode := Quarantined; end if;
   end Discard_Unbound;
   procedure Initialize (S : in out State; Context : in out C.State;
      Submission : V.State; Request : System.Address; Size : Capacity_Range;
      Budget : in out A.State; Accepted : out Boolean)
   is
      Bytes : F.U64; Types, Result : F.U32; Selected : F.U32 := 32;
      Address : System.Address;
   begin
      Accepted := False;
      if S.Mode not in Fresh | Closed or else S.Parent /= C.No_Child or else
         A.Current (Budget, S.Ticket) or else C.Current (Context) /= C.Live or else
         C.Context (Context) /= V.Owner_Context (Submission) or else Request = System.Null_Address
      then return; end if;
      C.Register_Child (Context, S.Parent);
      if S.Parent = C.No_Child then return; end if;
      S.Request := Request; S.Ticket := A.No_Ticket; S.Mapped := System.Null_Address; S.Size := 0;
      F.Prepare (Request, F.U32 (Size), Bytes, Types, Result);
      if Result = 1 then Retire (S, Context); return;
      elsif Result /= 0 then S.Mode := Quarantined; return;
      end if;
      if Bytes < F.U64 (Size) or else Bytes > F.U64 (Natural'Last) or else Types = 0 then
         Discard_Unbound (S, Context); return;
      end if;
      for N in Natural range 0 .. 31 loop
         if (Types and Interfaces.Shift_Left (F.U32 (1), N)) /= 0 then Selected := F.U32 (N); exit; end if;
      end loop;
      if Selected = 32 then Discard_Unbound (S, Context); return; end if;
      A.Reserve (Budget, Positive (Bytes), S.Ticket);
      if S.Ticket = A.No_Ticket then Discard_Unbound (S, Context); return; end if;
      F.Bind (Request, Bytes, Selected, Address, Result);
      if Result = 0 and then Address /= System.Null_Address then
         A.Allocated (Budget, S.Ticket, True);
         S.Mode := Live; S.Size := Size; S.Mapped := Address; Accepted := True;
      elsif Result = 1 then
         A.Allocated (Budget, S.Ticket, True);
         A.Begin_Release (Budget, S.Ticket, True); A.Released (Budget, S.Ticket, True);
         Retire (S, Context);
      else
         A.Allocated (Budget, S.Ticket, False); S.Mode := Quarantined;
      end if;
   end Initialize;
   procedure Close (S : in out State; Context : in out C.State;
      Budget : in out A.State; Readers_Retired : Boolean; Released : out Boolean)
   is
      Result : F.U32;
   begin
      Released := False;
      if not Readers_Retired or else S.Mode /= Live or else not Parent_Held (S, Context) or else
         not A.Current (Budget, S.Ticket) or else A.Status (Budget, S.Ticket) /= A.Live then return; end if;
      A.Begin_Release (Budget, S.Ticket, True);
      F.Release (S.Request, Result); A.Released (Budget, S.Ticket, Result = 0);
      if Result = 0 then Retire (S, Context); Released := True;
      else S.Mode := Quarantined;
      end if;
   end Close;
end Vulkan_Upload_Owner;
