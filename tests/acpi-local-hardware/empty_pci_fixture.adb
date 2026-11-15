pragma Ada_2022;
package body Empty_PCI_Fixture is
   procedure Prepare (Key : P.Bus_Key; Deadline : R.Word; Result : out R.Window_Description) is
   begin Result := (others => <>); raise Program_Error with "unexpected PCI setup"; end Prepare;
   procedure Inspect (Epoch,ID : R.Word; Result : out R.Window_Description) is
   begin Result := (others => <>); raise Program_Error with "unexpected PCI inspect"; end Inspect;
   procedure Read_Data (Epoch,ID : R.Word; Region : P.Region; Offset : R.Word; Width : P.T.Access_Width;
      Deadline : R.Word; Guard : not null access function return Boolean; Result : out R.Result) is
   begin Result := (others => <>); raise Program_Error with "unexpected PCI data"; end Read_Data;
   procedure Deadlines (Setup,Data : out R.Word; Available : out Boolean) is
   begin Setup := 0; Data := 0; Available := False; end Deadlines;
end Empty_PCI_Fixture;
