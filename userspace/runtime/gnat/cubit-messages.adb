------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2021 Jon Andrew
--
--  @summary
--  IPC Messages / Syscalls
--
--  Full multi-word IPC wrappers matching kernel Process.IPC.
------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with System;
with System.Machine_Code; use System.Machine_Code;

package body CuBit.Messages is

   --  syscall
   --  Ada interface to syscall instruction (x86_64)
   --
   --  Note: The syscall instruction clobbers RCX (return address) and
   --  R11 (RFLAGS). The kernel entry moves R10 -> RCX for arg3, so arg3
   --  must go into R10 from userspace.

   function syscall
     (call : Unsigned_64; arg0 : Unsigned_64 := 0; arg1 : Unsigned_64 := 0;
      arg2 : Unsigned_64 := 0; arg3 : Unsigned_64 := 0;
      arg4 : Unsigned_64 := 0; arg5 : Unsigned_64 := 0) return Unsigned_64
   is
      use ASCII;

      ret : Unsigned_64;
   begin
      Asm
        ("mov %0, %%rax" & LF & "mov %1, %%rdi" & LF & "mov %2, %%rsi" & LF &
         "mov %3, %%rdx" & LF & "mov %4, %%r10" & LF & "mov %5, %%r8" & LF &
         "mov %6, %%r9" & LF & "syscall",
         Inputs =>
           (Unsigned_64'Asm_Input ("g", call),
            Unsigned_64'Asm_Input ("g", arg0),
            Unsigned_64'Asm_Input ("g", arg1),
            Unsigned_64'Asm_Input ("g", arg2),
            Unsigned_64'Asm_Input ("g", arg3),
            Unsigned_64'Asm_Input ("g", arg4),
            Unsigned_64'Asm_Input ("g", arg5)),
         Clobber => "rax, rdi, rsi, rdx, r10, rcx, r8, r9, r11",
         Volatile => True);

      Asm
        ("mov %%rax, %0" & LF,
         Outputs => (Unsigned_64'Asm_Output ("=g", ret)),
         Clobber => "memory",
         Volatile => True);

      return ret;
   end syscall;

   --  Conversion helpers

   function tagToU64 is new Ada.Unchecked_Conversion
      (MessageTag, Unsigned_64);
   function u64ToTag is new Ada.Unchecked_Conversion
      (Unsigned_64, MessageTag);
   function toNum is new Ada.Unchecked_Conversion
      (System.Address, Unsigned_64);

   --  send
   --  SEND: RDI=dest, RSI=tag, RDX=w0, R10=w1, R8=w2, R9=w3
   --  Returns: reply tag in RAX

   function send (dest : ProcessID; msg : Message) return MessageTag is
      retTag : Unsigned_64;
   begin
      retTag := syscall (SYSCALL_SEND,
                          dest,
                          tagToU64 (msg.tag),
                          msg.words (0),
                          msg.words (1),
                          msg.words (2),
                          msg.words (3));
      return u64ToTag (retTag);
   end send;

   --  receive
   --  RECEIVE: RDI=pointer to Message struct
   --  Returns: RAX=sender PID

   procedure receive (from : out ProcessID; msg : out Message) is
   begin
      from := syscall (SYSCALL_RECEIVE, toNum (msg'Address));
   end receive;

   --  reply
   --  REPLY: RDI=dest, RSI=tag, RDX=w0, R10=w1, R8=w2, R9=w3

   function reply
     (replyTo : ProcessID; msg : Message) return Unsigned_64
   is
   begin
      return syscall (SYSCALL_REPLY,
                       replyTo,
                       tagToU64 (msg.tag),
                       msg.words (0),
                       msg.words (1),
                       msg.words (2),
                       msg.words (3));
   end reply;

   --  receiveNB
   --  RECEIVE_NB: RDI=pointer to Message struct
   --  Returns: RAX=sender PID (0 if no message)

   procedure receiveNB
     (from  : out ProcessID;
      msg   : out Message;
      found : out Boolean)
   is
      ret : Unsigned_64;
   begin
      ret := syscall (SYSCALL_RECEIVE_NB, toNum (msg'Address));
      from := ret;
      found := (ret /= 0);
   end receiveNB;

   --  submit
   --  SUBMIT: RDI=dest, RSI=tag, RDX=w0, R10=w1, R8=w2, R9=token
   --  Note: w3 is sacrificed to pass token in R9

   function submit
     (dest  : ProcessID;
      msg   : Message;
      token : Unsigned_64) return Boolean
   is
      ret : Unsigned_64;
   begin
      ret := syscall (SYSCALL_SUBMIT,
                       dest,
                       tagToU64 (msg.tag),
                       msg.words (0),
                       msg.words (1),
                       msg.words (2),
                       token);
      return (ret = 1);
   end submit;

   --  sendEvent
   --  SEND_EVENT: RDI=dest, RSI=tag, RDX=w0, R10=w1, R8=w2, R9=w3

   procedure sendEvent (dest : ProcessID; msg : Message) is
      ignore : Unsigned_64;
   begin
      ignore := syscall (SYSCALL_SEND_EVENT,
                          dest,
                          tagToU64 (msg.tag),
                          msg.words (0),
                          msg.words (1),
                          msg.words (2),
                          msg.words (3));
   end sendEvent;

   --  receiveEvent
   --  RECEIVE_EVENT: no args
   --  Returns: event tag in RAX

   function receiveEvent return Message is
      retTag : Unsigned_64;
      msg : Message := NULL_MESSAGE;
   begin
      retTag := syscall (SYSCALL_RECEIVE_EVENT);
      msg.tag := u64ToTag (retTag);
      return msg;
   end receiveEvent;

   --  createGrant
   --  GRANT: RDI=grantee, RSI=localAddr, RDX=numPages, R10=permission
   --  Returns: grant_id (or -1 on error)

   procedure createGrant
     (grantee   : ProcessID;
      localAddr : System.Address;
      numPages  : Natural;
      readWrite : Boolean;
      grantId   : out Unsigned_64;
      success   : out Boolean)
   is
      perm : Unsigned_64 := 0;
      ret  : Unsigned_64;
   begin
      if readWrite then
         perm := 1;
      end if;

      ret := syscall (SYSCALL_GRANT,
                       grantee,
                       toNum (localAddr),
                       Unsigned_64 (numPages),
                       perm);
      if ret = Unsigned_64'Last then
         grantId := 0;
         success := False;
      else
         grantId := ret;
         success := True;
      end if;
   end createGrant;

   --  revokeGrant
   --  REVOKE: RDI=grant_id

   procedure revokeGrant (id : Unsigned_64) is
      ignore : Unsigned_64;
   begin
      ignore := syscall (SYSCALL_REVOKE, id);
   end revokeGrant;

   --  Legacy wrappers

   function sendMsg (to : Unsigned_64; msg : Unsigned_64)
      return Unsigned_64
   is
   begin
      return syscall (SYSCALL_SEND, to, msg);
   end sendMsg;

   function recvMsg (from : out Unsigned_64) return Unsigned_64 is
      retfrom : Unsigned_64;
      pragma Unreferenced (from);
   begin
      return syscall (SYSCALL_RECEIVE, toNum (retfrom'Address));
   end recvMsg;

   function getInfo
     (query : Unsigned_64; detail : Unsigned_64 := 0) return Unsigned_64
   is
   begin
      return syscall (SYSCALL_INFO, query, detail);
   end getInfo;

   function registerDriver (driver : Unsigned_64) return Unsigned_64 is
   begin
      return syscall (SYSCALL_REGISTER_DRIVER, driver);
   end registerDriver;

   procedure debugPrint (str : String) is
      ignore : Unsigned_64;
   begin
      ignore :=
         syscall (SYSCALL_WRITE, STDOUT, toNum (str'Address), str'Length);
   end debugPrint;

   function getSecondaryStack return System.Secondary_Stack.SS_Stack_Ptr
   is
      SYSINFO_SECONDARY_STACK_START : constant Unsigned_64 := 1001;

      function toPtr is
         new Ada.Unchecked_Conversion
            (Source => Unsigned_64,
             Target => System.Secondary_Stack.SS_Stack_Ptr);
   begin
      return toPtr (getInfo (SYSINFO_SECONDARY_STACK_START));
   end getSecondaryStack;

   --  Port I/O wrappers

   function portIn8 (port : Unsigned_16) return Unsigned_64 is
   begin
      return syscall (SYSCALL_INB, Unsigned_64 (port));
   end portIn8;

   function portOut8
     (port : Unsigned_16; val : Unsigned_8) return Unsigned_64
   is
   begin
      return syscall (SYSCALL_OUTB, Unsigned_64 (port), Unsigned_64 (val));
   end portOut8;

   function portIn16 (port : Unsigned_16) return Unsigned_64 is
   begin
      return syscall (SYSCALL_INW, Unsigned_64 (port));
   end portIn16;

   function portOut16
     (port : Unsigned_16; val : Unsigned_16) return Unsigned_64
   is
   begin
      return syscall (SYSCALL_OUTW, Unsigned_64 (port), Unsigned_64 (val));
   end portOut16;

   function portIns16
     (port  : Unsigned_16;
      addr  : System.Address;
      count : Unsigned_32) return Unsigned_64
   is
   begin
      return syscall (SYSCALL_INS16,
                      Unsigned_64 (port),
                      toNum (addr),
                      Unsigned_64 (count));
   end portIns16;

end CuBit.Messages;
