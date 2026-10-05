"""Delayed asynchronous Display lease evidence, using real native IPC.

The probes withhold completion records; they do not simulate GPU fences.
"""
import output_retirement_fixture as base


def instrument(source, mode):
    if mode not in ('scaled', 'shutdown', 'partial'):
        raise ValueError(mode)
    s = base.instrument(source, 'scaled')
    def rep(old, new):
        nonlocal s
        assert s.count(old) == 1, (old[:80], s.count(old))
        s = s.replace(old, new)
    rep('probeRendererPolls (Output) <= 3 or else probeInputDuring = 0 or else', 'probeRendererPolls (Output) <= 3 or else')
    rep('   primaryOutput : Output_Index := 0;', '   probeLeaseSaved : array (Output_Index) of CompletionEntry := (others => NULL_COMPLETION);\n   probeLeaseHeld, probeLeaseSeen, probeLeaseDelivered : array (Output_Index) of Boolean := (others => False);\n   probeLeasePolls, probeLeaseInput, probeLeaseAttempts, probeLeaseSubmitted : array (Output_Index) of Natural := (others => 0);\n   function Probe_Lease_Poll (C : in out CompletionEntry) return Unsigned_64 is\n      Count : Unsigned_64;\n   begin\n      for O in Output_Index loop\n         if probeLeaseHeld (O) then\n            probeLeasePolls (O) := probeLeasePolls (O) + 1;\n            if probeLeasePolls (O) > 2000 then\n               debugPrint ("LEASE-NATIVE: FAIL input or reply progress" & LF); exitCompositor (1);\n            end if;\n            if probeLeasePolls (O) > (if O = 0 then 3 else 7) and then probeInputDuring > probeLeaseInput (O) then\n               C := probeLeaseSaved (O); probeLeaseHeld (O) := False; probeLeaseDelivered (O) := True;\n               debugPrint ("LEASE-NATIVE: replay output=" & O\'Image & LF); return 1;\n            end if;\n         end if;\n      end loop;\n      Count := Poll_Completion (C\'Address);\n      if Count = 1 and then probeStarted and then not probeFinished then\n         for O in Output_Index loop\n            if C.token /= 0 and then C.token = LR.Token (outputLeaseRequests (O)) then\n               if probeLeaseSeen (O) then debugPrint ("LEASE-NATIVE: FAIL duplicate reply" & LF); exitCompositor (1); end if;\n               probeLeaseSeen (O) := True; probeLeaseHeld (O) := True;\n               probeLeaseSaved (O) := C; probeLeaseInput (O) := probeInputDuring;\n               debugPrint ("LEASE-NATIVE: held output=" & O\'Image & LF);\n               if (for all Held of probeLeaseHeld => Held) then debugPrint ("LEASE-NATIVE: both replies held" & LF); end if;\n               return 0;\n            end if;\n         end loop;\n      end if;\n      return Count;\n   end Probe_Lease_Poll;\n   function Probe_Lease_Submit (Slot : CapabilitySlot; Request : Message; Token : Unsigned_64) return Boolean is\n      O : constant Output_Index := Output_Index (Request.tag.reserved);\n      Accepted : Boolean;\n   begin\n      probeLeaseAttempts (O) := probeLeaseAttempts (O) + 1;\n      if probeLeaseAttempts (O) <= 3 then return False; end if;\n      Accepted := capSubmit (Slot, Request, Token);\n      if Accepted then\n         probeLeaseSubmitted (O) := probeLeaseSubmitted (O) + 1;\n         if probeLeaseSubmitted (O) /= 1 then debugPrint ("LEASE-NATIVE: FAIL duplicate submit" & LF); exitCompositor (1); end if;\n      end if;\n      return Accepted;\n   end Probe_Lease_Submit;\n   primaryOutput : Output_Index := 0;')
    rep("count := Poll_Completion (completion'Address);", 'count := Probe_Lease_Poll (completion);')
    rep('capSubmit (CAP_SLOT_DISPLAY, Request, Token));', 'Probe_Lease_Submit (CAP_SLOT_DISPLAY, Request, Token));')
    rep('      if Output_Retirement.Status (R) = Output_Retirement.Grants_Pending then', '      if Output_Retirement.Status (R) = Output_Retirement.Grants_Pending then\n         if probeStarted and then not probeFinished and then not probeLeaseDelivered (Output) then\n            debugPrint ("LEASE-NATIVE: FAIL early grant revocation" & LF); exitCompositor (1);\n         end if;')
    rep('         probeFinished := True;', '         if (for some Done of probeLeaseDelivered => not Done) or else\n           (for some N of probeLeaseSubmitted => N /= 1)\n         then debugPrint ("LEASE-NATIVE: FAIL incomplete release evidence" & LF); exitCompositor (1); end if;\n         debugPrint ("LEASE-NATIVE: PASS both leases retired after input" & LF);\n         probeFinished := True;')
    if mode == 'shutdown':
        def rep(a, b):
            nonlocal s
            assert s.count(a) == 1, (a[:60], s.count(a))
            s = s.replace(a, b)
        rep('if count = 1 and then probeStarted and then outputDrainRequested and then', 'if count = 1 and then probeStarted and then')
        rep('if not probeStarted and then probeSawInput and then backBufferReady and then\n           (for some P of presentations => P.Enabled and then\n             P.Geometry.Scale.Numerator /= P.Geometry.Scale.Denominator) and then', 'if not probeStarted and then backBufferReady and then')
        rep('debugPrint ("OUTPUT-DRAIN: starting with a real queued frame" & LF);\n            releaseDisplayBuffer;', 'debugPrint ("OUTPUT-DRAIN: starting with a real queued frame" & LF);\n            running := False;')
        rep('PS.Charged (pixelStorage) /= 0 or else not outputReopenPending or else', 'PS.Charged (pixelStorage) /= 0 or else outputReopenPending or else not shutdownRequested or else')
        rep('   if fbBpp = 32 then\n      declare\n         cleared', '   if not probeFinished or else not shutdownRequested or else outputDrainRequested or else\n     outputReopenPending or else sourceRetirementPending or else backBufferReady or else\n     PS.Charged (pixelStorage) /= 0\n   then debugPrint ("OUTPUT-SHUTDOWN: FAIL exit before retirement" & LF); exitCompositor (1); end if;\n   debugPrint ("OUTPUT-SHUTDOWN: PASS drained loop exited" & LF);\n   if fbBpp = 32 then\n      declare\n         cleared')
        rep(' and then probeInputDuring > probeLeaseInput (O)', '')
        rep('probeInputDuring = 0 or else probeLate = 0', 'probeLate = 0')
        rep('LEASE-NATIVE: PASS both leases retired after input', 'LEASE-NATIVE: PASS both leases retired after confirmation')
    if mode == 'partial':
        def rep(a, b):
            nonlocal s
            assert s.count(a) == 1, (a[:80], s.count(a))
            s = s.replace(a, b)
        rep('probeRendererPolls (Output) <= 3 or else\n              probeLate = 0', 'probeRendererPolls (Output) <= 7')
        rep(' and then probeInputDuring > probeLeaseInput (O)', '')
        rep('if probeGrantPolls (Output) (B) <= (if Output = 0 then 3 else 7) then\n                  debugPrint', 'if P.Targets (B).Granted and then probeGrantPolls (Output) (B) <= (if Output = 0 then 3 else 7) then\n                  debugPrint')
        rep('PS.Charged (pixelStorage) /= 0 or else not outputReopenPending or else\n           probeInputDuring = 0 or else probeLate = 0 or else not probeReplayed', 'PS.Charged (pixelStorage) /= 0 or else outputReopenPending or else\n           backBufferReady or else probeCharge = 0')
        rep('LEASE-NATIVE: PASS both leases retired after input', 'LEASE-NATIVE: PASS both leases retired after confirmation')
        start = s.index('         if not probeStarted and then probeSawInput and then backBufferReady and then')
        end = s.index('         end if;', start) + len('         end if;')
        s = s[:start] + s[end:]
        rep('         P.Targets (B).Address := To_Address (Integer_Address (Raw));\n         MG.Create_Via_Capability', '         P.Targets (B).Address := To_Address (Integer_Address (Raw));\n         if Output = 1 and then B = 2 and then not probeStarted then\n            probeStarted := True; probeCharge := PS.Charged (pixelStorage);\n            for O in Output_Index loop probeTokens (O) := CP.Token (presentations (O).Transfer); end loop;\n            if backBufferReady or else not P.Leased or else not P.Targets (1).Granted or else\n              P.Targets (2).Granted or else P.Targets (2).Allocation = PS.No_Ticket\n            then debugPrint ("OUTPUT-PARTIAL: FAIL invalid injection point" & LF); exitCompositor (1); end if;\n            debugPrint ("OUTPUT-PARTIAL: allocated ungranted target; aborting setup" & LF);\n            closeOutput (Output); return;\n         end if;\n         MG.Create_Via_Capability')
        rep('         pumpOutputRetirements;\n         if outputReopenPending', '         pumpOutputRetirements;\n         if probeFinished and then not probeReopened then\n            declare Ready : Boolean; begin\n               activateInternalSession (Ready);\n               if not Ready or else not backBufferReady or else outputDrainRequested then\n                  debugPrint ("OUTPUT-PARTIAL: FAIL recovery" & LF); exitCompositor (1);\n               end if;\n               probeReopened := True;\n               debugPrint ("OUTPUT-PARTIAL: PASS recovered internal session" & LF);\n            end;\n         end if;\n         if outputReopenPending')
    return s


def check(serial, mode):
    base.check(serial, mode)
    if any(marker in serial for marker in ('LEASE-NATIVE: FAIL', 'output lease completion uncertain', 'output lease identifiers exhausted')):
        raise ValueError('native lease assertion failed')
    success = 'LEASE-NATIVE: PASS both leases retired after ' + ('input' if mode == 'scaled' else 'confirmation')
    if serial.count(success) != 1:
        raise ValueError('missing or duplicate lease retirement')
    commit = serial.index(success)
    for output in (0, 1):
        held = 'LEASE-NATIVE: held output= ' + str(output)
        replay = 'LEASE-NATIVE: replay output= ' + str(output)
        if serial.count(held) != 1 or serial.count(replay) != 1 or not serial.index(held) < serial.index(replay) < commit:
            raise ValueError('missing, duplicate or reordered lease evidence')
    if commit >= serial.index('OUTPUT-DRAIN: PASS drained'):
        raise ValueError('cleanup preceded lease retirement')
    if mode == 'scaled':
        marker = 'LEASE-NATIVE: both replies held'
        if serial.count(marker) != 1 or not all(
                serial.index('LEASE-NATIVE: held output= ' + str(o)) < serial.index(marker) <
                serial.index('LEASE-NATIVE: replay output= ' + str(o)) for o in (0, 1)):
            raise ValueError('input barrier did not hold both replies')


def self_test():
    frames = ('OUTPUT-DRAIN: starting with a real queued frame\n'
              'OUTPUT-DRAIN: holding real completion\nOUTPUT-DRAIN: delivering held completion\n')
    leases = ('LEASE-NATIVE: held output= 0\nLEASE-NATIVE: held output= 1\n'
              'LEASE-NATIVE: both replies held\nLEASE-NATIVE: replay output= 0\nLEASE-NATIVE: replay output= 1\n')
    rejected = 0
    for mode in ('scaled', 'shutdown', 'partial'):
        first = ('OUTPUT-PARTIAL: allocated ungranted target; aborting setup\n' if mode == 'partial' else frames)
        good = 'Loaded module desktop.svc w/ process ID 32\n' + first + leases
        good += 'LEASE-NATIVE: PASS both leases retired after ' + ('input' if mode == 'scaled' else 'confirmation') + '\n'
        good += 'OUTPUT-DRAIN: PASS drained input= 2 requests= 0 late= 2\n'
        good += {'scaled': 'OUTPUT-DRAIN: PASS restored scaled layout\nOUTPUT-DRAIN: PASS reopened\n',
                 'shutdown': 'OUTPUT-SHUTDOWN: PASS drained loop exited\nProcess.reclaimProcess: stopped PID 32\n',
                 'partial': 'OUTPUT-PARTIAL: PASS recovered internal session\n'}[mode]
        check(good, mode)
        bad = [good.replace(line, '', 1) for line in good.splitlines(True) if line.startswith('LEASE-NATIVE:') and
               (mode == 'scaled' or 'both replies held' not in line)]
        bad += [good + 'desktop: output lease completion uncertain\n', good + 'desktop: output lease identifiers exhausted\n', good + 'LEASE-NATIVE: FAIL early revoke\n', good.replace('held output= 1', 'held output= 0'),
                good.replace('LEASE-NATIVE: held output= 0', 'LEASE-NATIVE: replay output= 0', 1)]
        for text in bad:
            try: check(text, mode)
            except ValueError: rejected += 1; continue
            raise AssertionError('bad lease evidence accepted')
    print('PASS async-lease oracle: three modes and', rejected, 'negative controls')
