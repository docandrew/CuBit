with Ada.Text_IO; with Lexicon_Channel; with IPCtest_Channel;
procedure Layout is
   procedure Put (N : Natural) is
   begin Ada.Text_IO.Put_Line (N'Image); end Put;
begin
   Put (Lexicon_Channel.Request'Size); Put (Lexicon_Channel.Result'Size);
   Put (Lexicon_Channel.RC.Request_Ring_Bytes); Put (Lexicon_Channel.RC.Completion_Ring_Bytes);
   Put (Lexicon_Channel.RC.Client_Pages); Put (Lexicon_Channel.RC.Server_Pages);
   Put (Lexicon_Channel.RC.Ring_At); Put (Lexicon_Channel.RC.Client_State_At); Put (Lexicon_Channel.RC.Status_At);
   Put (IPCtest_Channel.Request'Size); Put (IPCtest_Channel.Result'Size);
   Put (IPCtest_Channel.RC.Request_Ring_Bytes); Put (IPCtest_Channel.RC.Completion_Ring_Bytes);
   Put (IPCtest_Channel.RC.Client_Pages); Put (IPCtest_Channel.RC.Server_Pages);
   Put (IPCtest_Channel.RC.Ring_At); Put (IPCtest_Channel.RC.Client_State_At); Put (IPCtest_Channel.RC.Status_At);
end Layout;
