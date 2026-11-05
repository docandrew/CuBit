pragma Ada_2022;
with Interfaces; with Ada.Text_IO; with GNAT.Source_Info;
with ACPI_Startup_Timing; with ACPI_Inventory_Protocol; with ACPI_Global_Lock_Protocol;
with ACPI_Locked_NVS_Protocol; with ACPI_Region_Protocol;
procedure Startup_Timing_Tests is
   package P renames ACPI_Startup_Timing;
   use type P.Word, P.Kind;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Line : Positive := GNAT.Source_Info.Line) is
   begin Checks := Checks+1; if not Good then raise Program_Error with "check" & Checks'Image & " line" & Line'Image; end if; end Check;
begin
   declare C : P.Context; Root,Modu,Outer,Inner : P.Token; begin
      P.Start (C); P.Enter (C,P.Startup,0,True,Root); P.Enter (C,P.Module_Execution,10,True,Modu);
      P.Enter (C,P.NVS_Read,20,True,Outer); P.Enter (C,P.Lock_Acquire,25,True,Inner);
      P.Leave (C,Inner,35,True); P.Leave (C,Outer,50,True);
      P.Enter (C,P.AML_Sleep,60,True,Outer); P.Leave (C,Outer,70,True);
      P.Leave (C,Modu,90,True); P.Leave (C,Root,100,True); P.Stop (C);
      Check (P.Complete (C)); Check (P.Read (C,P.Startup).Total_US = 100);
      Check (P.Read (C,P.Module_Execution).Total_US = 80 and P.Read (C,P.Module_Execution).External_US = 40);
      Check (P.Read (C,P.NVS_Read).Total_US = 30 and P.Read (C,P.Lock_Acquire).Total_US = 10);
      Check (P.Read (C,P.Startup).External_US = 40);
      P.Start (C); P.Enter (C,P.NVS_Read,101,True,Outer); P.Leave (C,Outer,102,True);
      Check (not P.Active (C) and P.Read (C,P.NVS_Read).Calls = 1);
   end;
   declare C : P.Context; A,B : P.Token; begin
      P.Start (C); P.Enter (C,P.Startup,10,True,A); P.Enter (C,P.Scalar_Read,20,True,B);
      P.Leave (C,B,19,True); P.Leave (C,A,40,True); P.Stop (C);
      Check (not P.Complete (C) and P.Read (C,P.Startup).Samples = 0);
      Check (P.Read (C,P.Scalar_Read).Calls = 1 and P.Read (C,P.Scalar_Read).Samples = 0);
   end;
   declare C : P.Context; A : P.Token; begin
      P.Start (C); P.Enter (C,P.AML_Stall,0,False,A); P.Leave (C,A,P.Word'Last,True); P.Stop (C);
      Check (not P.Complete (C) and P.Open_Scopes (C) = 0);
   end;
   declare C : P.Context; A,B : P.Token; begin
      P.Start (C); P.Enter (C,P.Startup,0,True,A); P.Enter (C,P.NVS_Read,1,True,B);
      P.Leave (C,A,2,True); P.Leave (C,B,3,True); P.Stop (C);
      Check (not P.Complete (C) and P.Open_Scopes (C) = 1);
   end;
   declare C : P.Context; T : array (1 .. 17) of P.Token; begin
      P.Start (C);
      for I in T'Range loop P.Enter (C,P.Other_RPC,P.Word (I),True,T (I)); end loop;
      for I in reverse T'Range loop P.Leave (C,T (I),40,True); end loop;
      P.Stop (C); Check (not P.Complete (C) and P.Open_Scopes (C) = 0);
      Check (P.Read (C,P.Other_RPC).Calls = 17 and P.Read (C,P.Other_RPC).Samples = 16);
   end;
   declare C : P.Context; A,B,D : P.Token; begin
      P.Start (C); P.Enter (C,P.Startup,0,True,A); P.Enter (C,P.Other_RPC,10,True,B);
      P.Enter (C,P.Other_RPC,20,True,D); P.Leave (C,D,P.Word'Last-1,True);
      P.Leave (C,B,P.Word'Last,True); P.Leave (C,A,P.Word'Last,True); P.Stop (C);
      Check (not P.Complete (C)); Check (P.Read (C,P.Other_RPC).Total_US = P.Word'Last);
      Check (P.Read (C,P.Startup).External_US = P.Word'Last-10);
   end;
   declare C : P.Context; A : P.Token; begin
      P.Enter (C,P.Startup,1,True,A); P.Leave (C,A,2,True); Check (P.Clock_Samples (C) = 0);
      P.Start (C); P.Enter (C,P.Startup,3,True,A); P.Stop (C); Check (not P.Complete (C));
   end;
   Check (P.Classify (ACPI_Inventory_Protocol.Header_Operation) = P.Inventory_Header);
   Check (P.Classify (ACPI_Region_Protocol.Read_Operation) = P.Scalar_Read);
   declare
      package G renames ACPI_Global_Lock_Protocol; package D renames ACPI_Locked_NVS_Protocol;
      use type Interfaces.Unsigned_32, G.Operation;
   begin
      for K in G.Operation loop
         Check (P.Classify (G.First_Operation+G.Operation'Pos (K)) =
           (if K = G.Acquire then P.Lock_Acquire elsif K = G.Release then P.Lock_Release else P.Lock_Other));
      end loop;
      Check (P.Classify (D.First_Operation+D.Operation'Pos (D.Read_Datum)) = P.Scoped_Read);
      Check (P.Classify (G.First_Reply) = P.Other_RPC);
   end;
   Ada.Text_IO.Put_Line ("Startup timing policy checks" & Checks'Image);
end Startup_Timing_Tests;
