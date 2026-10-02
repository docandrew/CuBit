with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_State_Base; use Intel_GPU_ADLN_State_Base;
with Intel_GPU_ADLN_Pipe_Control;
with Intel_GPU_ADLN_State_Setup;
with Intel_GPU_ADLN_Pipeline;
with Intel_GPU_ADLN_Batch_Start;
with Intel_GPU_ADLN_Constants;
procedure State_Base_Tests is
   package PC renames Intel_GPU_ADLN_Pipe_Control;
   package Setup renames Intel_GPU_ADLN_State_Setup;
   package Constants renames Intel_GPU_ADLN_Constants;
   use type Constants.Words;
   function Alloc_Record is new Ada.Unchecked_Conversion (Unsigned_32, Constants.Allocation_Control);
   function Clear_Head is new Ada.Unchecked_Conversion (Unsigned_32, Constants.Clear_Header);
   function Clear_Flags is new Ada.Unchecked_Conversion (Unsigned_32, Constants.Clear_Control);
   package Pipeline renames Intel_GPU_ADLN_Pipeline;
   use type Pipeline.Packet;
   function Select_Word is new Ada.Unchecked_Conversion (Unsigned_32, Pipeline.Select_Control);
   use type PC.Packet;
   use type Setup.Words;
   function PC_Header is new Ada.Unchecked_Conversion (Unsigned_32, PC.Pipe_Header);
   function PC_Flags is new Ada.Unchecked_Conversion (Unsigned_32, PC.Pipe_Flags);
   function Base is new Ada.Unchecked_Conversion (Unsigned_64, Base_Control);
   function Stateless is new Ada.Unchecked_Conversion (Unsigned_32, Stateless_Control);
   function Bound is new Ada.Unchecked_Conversion (Unsigned_32, Page_Bound);
   function Surface is new Ada.Unchecked_Conversion (Unsigned_32, Bindless_Surface_Bound);
   function Sampler is new Ada.Unchecked_Conversion (Unsigned_32, Bindless_Sampler_Bound);
   Expected : constant Words :=
     [16#61010014#, 16#61#, 0, 16#60000#, 16#206061#, 0, 16#206061#, 0,
      16#61#, 0, 16#207061#, 0, 1, 16#1001#, 1, 16#1001#,
      16#61#, 0, 0, 16#61#, 0, 0];
   R : Image;
   S : Setup.Image;
   C : Constants.Image;
begin
   for Bit in 0 .. 63 loop
      declare V : constant Unsigned_64 := Shift_Left (1, Bit); begin
         pragma Assert (Encode (Base (V)) = V);
      end;
   end loop;
   for Bit in 0 .. 31 loop
      declare V : constant Unsigned_32 := Shift_Left (1, Bit); begin
         pragma Assert (Encode (Stateless (V)) = V);
         pragma Assert (Encode (Bound (V)) = V);
         pragma Assert (Encode (Surface (V)) = V);
         pragma Assert (Encode (Sampler (V)) = V);
         pragma Assert (PC.Encode (PC_Header (V)) = V);
         pragma Assert (PC.Encode (PC_Flags (V)) = V);
         pragma Assert (Pipeline.Encode (Select_Word (V)) = V);
         pragma Assert (Constants.Encode (Alloc_Record (V)) = V);
         pragma Assert (Constants.Encode (Clear_Head (V)) = V);
         pragma Assert (Constants.Encode (Clear_Flags (V)) = V);
      end;
   end loop;
   R := Build (6);
   pragma Assert (R.Valid and then R.Data = Expected);
   C := Constants.Build (6);
   pragma Assert (C.Valid and then C.Data = Constants.Words'
     [16#79120000#, 0, 16#79130000#, 0, 16#79140000#, 0,
      16#79150000#, 0, 16#79160000#, 0, 16#786D1F00#, 6]);
   pragma Assert (PC.Before_State_Base = PC.Packet'[16#7A000204#, 16#00101000#, 0, 0, 0, 0]);
   pragma Assert (PC.After_State_Base = PC.Packet'[16#7A000004#, 16#20000C0C#, 0, 0, 0, 0]);
   S := Setup.Build (6);
   pragma Assert (S.Valid);
   pragma Assert (Pipeline.Initial_3D = Pipeline.Packet'
     [16#7A000204#, 16#00103001#, 0, 0, 0, 0, 16#69041310#]);
   for I in Pipeline.Packet'Range loop
      pragma Assert (S.Data (I) = Pipeline.Initial_3D (I));
   end loop;
   declare
      Batch : constant Intel_GPU_ADLN_Batch_Start.Command_Words := Intel_GPU_ADLN_Batch_Start.Build;
   begin
      pragma Assert ((Batch (1) and 16#400#) = 0);
   end;
   for I in PC.Packet'Range loop
      pragma Assert (S.Data (7 + I) = PC.Before_State_Base (I));
      pragma Assert (S.Data (35 + I) = PC.After_State_Base (I));
   end loop;
   for I in Words'Range loop
      pragma Assert (S.Data (13 + I) = Expected (I));
   end loop;
   for M in Unsigned_32 range 0 .. 255 loop
      R := Build (M);
      C := Constants.Build (M);
      pragma Assert (C.Valid = R.Valid);
      if C.Valid then
         pragma Assert (C.Data (11) = M);
      else
         pragma Assert (C.Data = Constants.Words'(others => 0));
      end if;
      S := Setup.Build (M);
      pragma Assert (S.Valid = R.Valid);
      if M > 0 and M <= 126 and M mod 2 = 0 then
         pragma Assert (R.Valid);
         pragma Assert (R.Data (1) = 1 + Shift_Left (M, 4));
         pragma Assert (R.Data (3) = Shift_Left (M, 16));
         pragma Assert (R.Data (4) = 16#206001# + Shift_Left (M, 4));
         pragma Assert (R.Data (6) = R.Data (4));
         pragma Assert (R.Data (8) = R.Data (1));
         pragma Assert (R.Data (10) = 16#207001# + Shift_Left (M, 4));
         pragma Assert (R.Data (16) = R.Data (1));
         pragma Assert (R.Data (19) = R.Data (1));
      else
         pragma Assert (not R.Valid and then R.Data = Words'(others => 0));
         pragma Assert (S.Data = Setup.Words'(others => 0));
      end if;
   end loop;
   R := Build (Unsigned_32'Last);
   pragma Assert (not R.Valid);
   Put_Line ("state setup PASS: 384 record bits, setup/constant fixtures, streamer bit clear, MOCS admission");
end State_Base_Tests;
