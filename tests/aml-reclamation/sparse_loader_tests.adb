with Ada.Text_IO;
with AML_Data;
with AML_Decode;
with AML_Identity.Issuer;
with AML_Objects.Reclamation;
procedure Sparse_Loader_Tests is
   package O renames AML_Objects;
   package R renames AML_Objects.Reclamation;
   use type O.State;
   use type O.Allocation_Status;
   use type R.Reclaim_Status;
   use type AML_Decode.Status;
   use type AML_Decode.Integer_Value;
   Store : O.State := O.Empty;
   Scratch : R.Workspace;
   Keep : R.Keep_Set := [others => False];
   Owner : AML_Identity.Identity;
   Issued : Boolean;
   Alloc : O.Allocation_Status;
   Reclaimed : R.Reclaim_Status;
   Status : AML_Decode.Status;
   ID, Other : O.Object_ID;
   Consumed : Natural;
   Checks : Natural := 0;
   type Environment is record
      Target : O.Object_ID := 0;
      Calls : Natural := 0;
   end record;
   Context : Environment;
   function Count_Unsupported
     (E : Environment; Data : AML_Decode.Bytes; Width : AML_Decode.Integer_Width)
      return AML_Data.Count_Result is
      pragma Unreferenced (E, Data, Width);
   begin return (others => <>); end Count_Unsupported;
   procedure Member
     (E : in out Environment; Package_ID : O.Object_ID; Element : Natural;
      Data : AML_Decode.Bytes; Result : out AML_Data.Member_Result)
   is
      pragma Unreferenced (Package_ID, Element);
   begin
      E.Calls := E.Calls + 1;
      Result := (Kind => AML_Decode.Accepted, Binding => AML_Data.Resolved_Member,
        ID => E.Target, Consumed => Data'Length);
   end Member;
   procedure Load_Bound is new AML_Data.Load_Bound (Environment, Count_Unsupported, Member);
   procedure Check (Good : Boolean; Label_Text : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Label_Text; end if;
   end Check;
begin
   AML_Identity.Issuer.Issue (Owner, Issued); Check (Issued, "owner issuance");
   for I in 1 .. 6 loop
      O.New_Integer (Store, AML_Decode.Integer_Value (I), Other, Alloc);
      Check (Alloc = O.Allocated, "original integer");
      if I mod 2 = 0 then Keep (Other) := True; end if;
   end loop;
   R.Reclaim (Store, Owner, Keep, Scratch, Reclaimed);
   Check (Reclaimed = R.Reclaimed, "sparse source");
   AML_Data.Load (Store, [16#0A#,42], AML_Decode.Bits_64, ID, Consumed, Status);
   Check (Status = AML_Decode.Accepted and then ID = 1 and then Consumed = 2
     and then O.Integer_Data (Store, ID) = 42, "literal uses reclaimed slot");
   AML_Data.Load (Store, [16#12#,4,2,0,1], AML_Decode.Bits_64, ID, Consumed, Status);
   Check (Status = AML_Decode.Accepted and then ID = 3 and then Consumed = 5
     and then O.Element (Store, ID, 0) = 5 and then O.Element (Store, ID, 1) = 7,
     "package children use actual new IDs");
   declare Before : constant O.State := Store; begin
      AML_Data.Load (Store, [16#12#,4,2,0,16#0A#], AML_Decode.Bits_64, ID, Consumed, Status);
      Check (Status = AML_Decode.Truncated and then ID = 0 and then Consumed = 0
        and then Store = Before, "malformed package rollback");
   end;
   Store := O.Empty; Keep := [others => False];
   O.New_Integer (Store, 0, Other, Alloc); Check (Alloc = O.Allocated, "dead member slot");
   O.New_Integer (Store, 9, Other, Alloc); Check (Alloc = O.Allocated, "live member slot");
   Keep (Other) := True;
   R.Reclaim (Store, Owner, Keep, Scratch, Reclaimed); Check (Reclaimed = R.Reclaimed, "member sparse source");
   Context := (Target => 1, Calls => 0);
   declare Before : constant O.State := Store; Before_Context : constant Environment := Context; begin
      Load_Bound (Store, [16#12#,6,1,78,65,77,69], AML_Decode.Bits_64, Context, ID, Consumed, Status);
      Check (Status = AML_Decode.Malformed and then ID = 0 and then Consumed = 0
        and then Store = Before and then Context = Before_Context,
        "callback dead old slot cannot alias provisional root");
   end;
   Context.Target := Other;
   Load_Bound (Store, [16#12#,6,1,78,65,77,69], AML_Decode.Bits_64, Context, ID, Consumed, Status);
   Check (Status = AML_Decode.Accepted and then ID = 1 and then O.Element (Store, ID, 0) = Other
     and then Context.Calls = 1, "live member alias preserved");
   -- The caller may supply a legal byte slice whose first index is near the
   -- base integer limit. Offset arithmetic must not overflow transiently.
   declare Data : constant AML_Decode.Bytes (Positive'Last - 1 .. Positive'Last) := [16#0A#,42]; begin
      AML_Data.Load (Store, Data, AML_Decode.Bits_64, ID, Consumed, Status);
      Check (Status = AML_Decode.Accepted and then Consumed = 2
        and then O.Integer_Data (Store, ID) = 42, "high-bound byte slice");
   end;
   Ada.Text_IO.Put_Line ("SPARSE LOADER" & Checks'Image);
end Sparse_Loader_Tests;
