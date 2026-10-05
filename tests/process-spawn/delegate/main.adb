--  Delegated places (docs/self-hosting.md, item 4): what a launcher hands a
--  child through CuBit.Launching / CuBit.Launch_Grants, checked by procmgr
--  against what the launcher holds. Results go to the kernel console.
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Launch_Arguments;
with CuBit.Launch_Grants;
with CuBit.Launching;
with CuBit.Messages; use CuBit.Messages;

procedure Main is
   package LA renames CuBit.Launch_Arguments;
   package LG renames CuBit.Launch_Grants;
   use type CuBit.Launching.Launch_Result;
   use type LA.Launch_Failure;

   Note : constant String := "@nvme:0/delegated/note.txt";
   --  Longer than the 64 bytes a scope used to hold (run.sh creates it).
   Deep_Place : constant String :=
     "@nvme:0/delegated/a-directory-name-longer-than-the-old-sixty-four-byte-scope-limit";
   Failed : Boolean := False;

   procedure Say (Text : String; Pass : Boolean) is
   begin
      debugPrint ("delegate-check: " & Text & (if Pass then " PASS" else " FAIL") &
                  ASCII.LF);
      Failed := Failed or else not Pass;
   end Say;

   --  Start args-check MODE NOTE, delegating Rights on Place (none when
   --  Rights is 0).
   procedure Start
     (Mode : String; Rights : Unsigned_8; Place : String;
      Result : out CuBit.Launching.Launch_Result;
      Failure : out LA.Launch_Failure;
      Target : String := Note)
   is
      Block : LA.Builder;
      Length : LA.Present_Length;
      Accepted : Boolean;
      Grants : LG.Builder;
      Region : LG.Bytes (1 .. LG.Maximum_Bytes);
      Region_Length : LG.Byte_Count;
      Added : Boolean;
      Child : CuBit.Launching.Child;
   begin
      LA.Start (Block);
      LA.Add_Argument (Block, "args-check.app", Accepted);
      LA.Add_Argument (Block, Mode, Accepted);
      LA.Add_Argument (Block, Target, Accepted);
      LA.Finish (Block, Length, Accepted);
      LG.Start (Grants);
      if Rights /= 0 then
         LG.Add (Grants, Rights, Place, Added);
      end if;
      LG.Finish (Grants, Region, Region_Length);
      CuBit.Launching.Launch
        ("args-check.app", Block.Data (1 .. Length), Region (1 .. Region_Length),
         Child, Result, Failure);
   end Start;

   Result : CuBit.Launching.Launch_Result;
   Failure : LA.Launch_Failure;
begin
   --  Its own manifest does not let args-check read the place.
   Start ("denied", 0, "", Result, Failure);
   Say ("child started without delegation", Result = CuBit.Launching.Launched);
   --  Handed read access to the place, it can.
   Start ("read", LG.Read_Right, "@nvme:0/delegated", Result, Failure);
   Say ("child started with the place delegated", Result = CuBit.Launching.Launched);
   --  A place deeper than 64 bytes, delegated and read.
   Start ("read", LG.Read_Right, Deep_Place, Result, Failure,
          Target => Deep_Place & "/note.txt");
   Say ("a place longer than 64 bytes delegated", Result = CuBit.Launching.Launched);
   --  More rights than the launcher holds: refused, nothing started.
   Start ("read", LG.Read_Right or LG.Write_Right, "@nvme:0/delegated",
          Result, Failure);
   Say ("delegating write it does not hold is refused (Not_Granted)",
        Result = CuBit.Launching.Refused and then Failure = LA.Not_Granted);
   --  A place it does not hold at all: refused.
   Start ("read", LG.Read_Right, "@nvme:0/libc-check", Result, Failure);
   Say ("delegating a place it does not hold is refused (Not_Granted)",
        Result = CuBit.Launching.Refused and then Failure = LA.Not_Granted);
   debugPrint ((if Failed then "DELEGATE-CHECK: FAIL" else "DELEGATE-CHECK: PASS") &
               ASCII.LF);
end Main;
