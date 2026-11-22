-- SPDX-License-Identifier: MIT
-- Register layout adapted from Linux v6.16 intel_lrc.c.
-- Copyright (c) 2014 Intel Corporation
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.
package body Intel_GPU_ADLN_LRC_Template with SPARK_Mode is
   type Offsets is array (Positive range <>) of Unsigned_32;
   First : constant Offsets :=
     [16#244#,16#034#,16#030#,16#038#,16#03C#,16#168#,16#140#,
      16#110#,16#1C0#,16#1C4#,16#1C8#,16#180#,16#2B4#];
   Second : constant Offsets :=
     [16#3A8#,16#28C#,16#288#,16#284#,16#280#,16#27C#,16#278#,
      16#274#,16#270#];
   Third : constant Offsets := [16#1B0#,16#5A8#,16#5AC#];
   Fourth : constant Offsets := [1 => 16#0C8#];
   Fifth : constant Offsets :=
     [16#588#,16#588#,16#588#,16#588#,16#588#,16#588#,
      16#028#,16#09C#,16#0C0#,16#178#,16#17C#,16#358#,
      16#170#,16#150#,16#154#,16#158#,16#41C#,
      16#600#,16#604#,16#608#,16#60C#,16#610#,16#614#,16#618#,
      16#61C#,16#620#,16#624#,16#628#,16#62C#,16#630#,16#634#,
      16#638#,16#63C#,16#640#,16#644#,16#648#,16#64C#,16#650#,
      16#654#,16#658#,16#65C#,16#660#,16#664#,16#668#,16#66C#,
      16#670#,16#674#,16#678#,16#67C#,16#068#,16#084#];
   procedure Group (Page : in out Register_Page; Start : Natural;
                    Registers : Offsets; Posted : Boolean)
     with Pre => Registers'First = 1 and Registers'Length in 1 .. 51 and
                 Start <= 900
   is
   begin
      Page (Start) := 16#1108_0000# or
        (if Posted then 16#1000# else 0) or
        Unsigned_32 (Registers'Length * 2 - 1);
      for I in Registers'Range loop
         Page (Start + 1 + (I - 1) * 2) := 16#2000# + Registers (I);
      end loop;
   end Group;
   function Build return Register_Page is
      Page : Register_Page := [others => 0];
   begin
      Group (Page, 1, First, True);
      Group (Page, 33, Second, True);
      Group (Page, 52, Third, True);
      Group (Page, 65, Fourth, False);
      Group (Page, 81, Fifth, True);
      Page (185) := 16#0500_0000#; -- MI_BATCH_BUFFER_END for inhibited restore
      return Page;
   end Build;
end Intel_GPU_ADLN_LRC_Template;
