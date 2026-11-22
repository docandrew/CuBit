package body Vulkan_Owned_Source with SPARK_Mode is
   procedure Initialize
     (S : in out State; Context : in out C.State; Submission : V.State;
      Request : System.Address; Budget : in out A.State; Allowed_Types : I.U32;
      Accepted : out Boolean)
   is
      Reused : Boolean;
   begin
      Accepted := False;
      if C.Current (Context) /= C.Live or else Request = System.Null_Address or else
         C.Context (Context) /= V.Owner_Context (Submission) or else
         S.Parent /= C.No_Child or else Current (S) not in I.Fresh | I.Closed
      then return; end if;
      if Current (S) = I.Closed then
         I.Rearm (S.Image, Budget, Reused);
         if not Reused then return; end if;
      end if;
      C.Register_Child (Context, S.Parent);
      if S.Parent = C.No_Child then return; end if;
      S.Request := Request;
      I.Prepare (S.Image, Request);
      if Current (S) = I.Prepared then I.Allocate (S.Image, Budget, Allowed_Types); end if;
      if Current (S) = I.Closed then
         C.Retire_Child (Context, S.Parent, True);
         S.Parent := C.No_Child;
      end if;
      Accepted := Current (S) = I.Live;
   end Initialize;
   procedure Close
     (S : in out State; Context : in out C.State; Budget : in out A.State;
      All_Readers_Retired : Boolean; Released : out Boolean)
   is
   begin
      Released := False;
      if not All_Readers_Retired or else not Parent_Held (S, Context) or else
         not I.Can_Release (S.Image, Budget) then return; end if;
      I.Release (S.Image, Budget, True);
      if Current (S) = I.Closed then
         C.Retire_Child (Context, S.Parent, True);
         S.Parent := C.No_Child;
         Released := True;
      end if;
   end Close;
end Vulkan_Owned_Source;
