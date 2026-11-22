--  DP AUX against a sink model: DPCD reads, EDID over I2C-over-AUX (with
--  the E-DDC segment), DEFER retries, a receive error retried, NAK, timeout.
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Monitor_EDID.Catalog;
with EDID_Fixtures;
with AUX_Model;
with Intel_GPU_DP_AUX;
procedure AUX_Tests is
   package A is new Intel_GPU_DP_AUX (AUX_Model.Read, AUX_Model.Write, AUX_Model.Now_Us);
   use type A.Status, A.Bytes;
   package C renames CuBit.Monitor_EDID.Catalog;
   Panel : constant C.Block := EDID_Fixtures.FHD_EDP_Base;
   Image : AUX_Model.Byte_Array (0 .. 511) := [others => 0];
   Data : A.Bytes (1 .. 128);
   Caps : A.Bytes (1 .. 3);
   S : A.Status;
begin
   for I in 0 .. 127 loop Image (I) := Panel (I); Image (256 + I) := Unsigned_8 (255 - I); end loop;
   AUX_Model.Reset (Image);
   A.Read_DPCD (A.Channel_A, 0, Caps, S);
   pragma Assert (S = A.OK and then Caps = [16#14#, 16#1E#, 16#84#]);
   A.Read_EDID_Block (A.Channel_A, 0, Data, S);
   pragma Assert (S = A.OK);
   for I in 0 .. 127 loop pragma Assert (Data (I + 1) = Panel (I)); end loop;
   A.Read_EDID_Block (A.Channel_A, 2, Data, S);
   pragma Assert (S = A.OK and then Data (1) = 255 and then Data (128) = 128);
   AUX_Model.Defers := 5;
   A.Read_EDID_Block (A.Channel_A, 0, Data, S);
   pragma Assert (S = A.OK and then Data (9) = Panel (8));
   AUX_Model.Receive_Error_Once := True;
   A.Read_DPCD (A.Channel_A, 0, Caps, S);
   pragma Assert (S = A.OK);
   AUX_Model.Defers := 1_000;
   A.Read_DPCD (A.Channel_A, 0, Caps, S);
   pragma Assert (S = A.Defer_Exhausted);
   AUX_Model.Defers := 0;
   AUX_Model.Nak_All := True;
   A.Read_EDID_Block (A.Channel_A, 0, Data, S);
   pragma Assert (S = A.NAK);
   AUX_Model.Nak_All := False;
   AUX_Model.Never_Done := True;
   A.Read_DPCD (A.Channel_A, 0, Caps, S);
   pragma Assert (S = A.Timeout);
   Put_Line ("PASS: DP AUX DPCD and EDID over I2C-over-AUX, DEFER/receive-error retries, NAK, timeout");
end AUX_Tests;
