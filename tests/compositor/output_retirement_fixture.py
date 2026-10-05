"""Native output-retirement probes. Real calls precede delayed evidence.

These exercise Desktop control flow and real grants, not GPU fence truth.
"""
import re

def instrument(source, mode):
    if mode not in ('scaled', 'shutdown', 'partial'):
        raise ValueError(mode)
    s = source

    def rep(a, b):
        nonlocal s
        assert s.count(a) == 1, (a[:70], s.count(a))
        s = s.replace(a, b)
    rep('   primaryOutput : Output_Index := 0;', '   use type Desktop_Compositor.Target_Release;\n   use type DL.Layout, DG.Scale_Component;\n   probeExpectedLayout : DL.Layout;\n   probeStarted, probeFinished, probeReopened : Boolean := False;\n   probeSawInput : Boolean := False;\n   probeInputDuring, probeRequestsDuring, probeLate : Natural := 0;\n   probeRendererPolls : array (Output_Index) of Natural := (others => 0);\n   type Probe_Target_Counts is array (BP.Live_Slot) of Natural;\n   probeGrantPolls, probeRevokes : array (Output_Index) of Probe_Target_Counts := (others => (others => 0));\n   probeTokens : array (Output_Index) of Unsigned_64 := (others => 0);\n   probeCharge : Natural := 0;\n   probeSaved : CompletionEntry := NULL_COMPLETION;\n   probeSavedValid, probeReplayed : Boolean := False;\n   primaryOutput : Output_Index := 0;')
    rep("         count := Poll_Completion (completion'Address);", '         if probeSavedValid and then outputDrainRequested and then\n           (for all N of probeRendererPolls => N >= 3)\n         then\n            completion := probeSaved; count := 1;\n            probeSavedValid := False; probeReplayed := True;\n            debugPrint ("OUTPUT-DRAIN: delivering held completion" & LF);\n         else\n            count := Poll_Completion (completion\'Address);\n            if count = 1 and then probeStarted and then outputDrainRequested and then\n              not probeSavedValid and then not probeReplayed and then\n              (for some P of presentations => P.Enabled and then\n                completion.token = CP.Token (P.Transfer) and then completion.token /= 0)\n            then\n               probeSaved := completion; probeSavedValid := True; count := 0;\n               debugPrint ("OUTPUT-DRAIN: holding real completion" & LF);\n            end if;\n         end if;')
    rep('                  -- Continue validating completions while draining. Display', '                  if outputDrainRequested then probeLate := probeLate + 1; end if;\n                  -- Continue validating completions while draining. Display')
    rep('         Desktop_Compositor.Forget_Targets (Renderer);', '         Desktop_Compositor.Forget_Targets (Renderer);\n         if probeStarted and then not probeFinished and then Renderer = Desktop_Compositor.Targets_Retired then\n            probeRendererPolls (Output) := probeRendererPolls (Output) + 1;\n            if probeRendererPolls (Output) > 2000 then\n               debugPrint ("OUTPUT-DRAIN: FAIL no input progress while busy" & LF); exitCompositor (1);\n            end if;\n            if probeRendererPolls (Output) <= 3 or else probeInputDuring = 0 or else\n              probeLate = 0\n            then Renderer := Desktop_Compositor.Targets_Busy; end if;\n         end if;')
    rep('               MG.Revoke (P.Targets (B).Grant, Confirmed);', '               if probeStarted and then not probeFinished then\n                  probeRevokes (Output) (B) := probeRevokes (Output) (B) + 1;\n                  if probeRevokes (Output) (B) /= 1 then\n                     debugPrint ("OUTPUT-DRAIN: FAIL duplicate revoke" & LF); exitCompositor (1);\n                  end if;\n               end if;\n               MG.Revoke (P.Targets (B).Grant, Confirmed);')
    rep('               Output_Retirement.Observe_Grant\n                 (R, Positive (B), MG.Retirement_Confirmed (P.Targets (B).Grant));', '               Confirmed := MG.Retirement_Confirmed (P.Targets (B).Grant);\n               if probeStarted and then not probeFinished and then Confirmed then\n                  probeGrantPolls (Output) (B) := probeGrantPolls (Output) (B) + 1;\n                  if probeGrantPolls (Output) (B) <= (if Output = 0 then 3 else 7) then\n                     Confirmed := False;\n                  end if;\n               end if;\n               Output_Retirement.Observe_Grant (R, Positive (B), Confirmed);')
    rep('      if Output_Retirement.Status (R) = Output_Retirement.Storage_Ready then', '      if Output_Retirement.Status (R) = Output_Retirement.Storage_Ready then\n         if probeStarted and then not probeFinished then\n            for B in BP.Live_Slot loop\n               if probeGrantPolls (Output) (B) <= (if Output = 0 then 3 else 7) then\n                  debugPrint ("OUTPUT-DRAIN: FAIL early storage release" & LF); exitCompositor (1);\n               end if;\n            end loop;\n         end if;')
    rep('      -- Renderer, lease and grant confirmations have arrived for every output.', '      if probeStarted and then not probeFinished then\n         for Output in Output_Index loop\n            if CP.Token (presentations (Output).Transfer) /= probeTokens (Output) then\n               debugPrint ("OUTPUT-DRAIN: FAIL completion identity discarded" & LF); exitCompositor (1);\n            end if;\n         end loop;\n      end if;\n      probeExpectedLayout := desktopLayout;\n      -- Renderer, lease and grant confirmations have arrived for every output.')
    rep('      -- Do not clear input queues here:', '      if probeStarted and then not probeFinished then\n         if PS.Charged (pixelStorage) /= 0 or else not outputReopenPending or else\n           probeInputDuring = 0 or else probeLate = 0 or else not probeReplayed\n         then debugPrint ("OUTPUT-DRAIN: FAIL cleanup commit" & LF); exitCompositor (1); end if;\n         probeFinished := True;\n         debugPrint ("OUTPUT-DRAIN: PASS drained input=" & probeInputDuring\'Image &\n           " requests=" & probeRequestsDuring\'Image & " late=" & probeLate\'Image & LF);\n      end if;\n      -- Do not clear input queues here:')
    rep('                  handleEvent (eventMsg, running);', '                  probeSawInput := True;\n                  if outputDrainRequested then probeInputDuring := probeInputDuring + 1; end if;\n                  handleEvent (eventMsg, running);')
    rep('                  handleRequest (from, msg);', '                  if outputDrainRequested then probeRequestsDuring := probeRequestsDuring + 1; end if;\n                  handleRequest (from, msg);')
    rep('               if Ready then scheduleRedraw; end if;', '               if Ready then\n                  scheduleRedraw;\n                  if probeFinished and then not probeReopened then\n                     probeReopened := True;\n                     if desktopLayout /= probeExpectedLayout then\n                        debugPrint ("OUTPUT-DRAIN: FAIL lost DPI or arrangement" & LF); exitCompositor (1);\n                     end if;\n                     debugPrint ("OUTPUT-DRAIN: PASS restored scaled layout" & LF);\n                     if PS.Charged (pixelStorage) /= probeCharge then\n                        debugPrint ("OUTPUT-DRAIN: FAIL reopen memory charge" & LF); exitCompositor (1);\n                     end if;\n                     debugPrint ("OUTPUT-DRAIN: PASS reopened" & LF);\n                  end if;\n               end if;')
    rep('         pumpSourceRetirements;', '         pumpSourceRetirements;\n         if not probeStarted and then probeSawInput and then backBufferReady and then\n           (for some P of presentations => P.Enabled and then\n             P.Geometry.Scale.Numerator /= P.Geometry.Scale.Denominator) and then\n           (for all P of presentations => P.Enabled and then CP.Token (P.Transfer) /= 0) and then\n           (for some P of presentations => CP.Current (P.Transfer) = CP.In_Flight)\n         then\n            probeStarted := True;\n            probeCharge := PS.Charged (pixelStorage);\n            for Output in Output_Index loop probeTokens (Output) := CP.Token (presentations (Output).Transfer); end loop;\n            debugPrint ("OUTPUT-DRAIN: starting with a real queued frame" & LF);\n            releaseDisplayBuffer;\n         end if;')
    rep("               activity := Wait_For_Activity_Until (Unsigned_64'Last);", '               if outputDrainRequested or else outputReopenPending then\n                  debugPrint ("OUTPUT-DRAIN: FAIL indefinite wait" & LF); exitCompositor (1);\n               end if;\n               activity := Wait_For_Activity_Until (Unsigned_64\'Last);')
    rep('               Output_Retirement.Observe_Revoke (R, Positive (B), Confirmed);', '               if not Confirmed then\n                  debugPrint ("OUTPUT-DRAIN: revoke failed output=" & Output\'Image & " buffer=" & B\'Image & LF);\n               end if;\n               Output_Retirement.Observe_Revoke (R, Positive (B), Confirmed);')
    if mode == 'shutdown':

        def change(a, b):
            nonlocal s
            assert s.count(a) == 1, (a[:90], s.count(a))
            s = s.replace(a, b)
        change('if count = 1 and then probeStarted and then outputDrainRequested and then', 'if count = 1 and then probeStarted and then')
        change('probeRendererPolls (Output) <= 3 or else probeInputDuring = 0 or else', 'probeRendererPolls (Output) <= 3 or else')
        change('PS.Charged (pixelStorage) /= 0 or else not outputReopenPending or else\n           probeInputDuring = 0 or else probeLate = 0 or else not probeReplayed', 'PS.Charged (pixelStorage) /= 0 or else outputReopenPending or else\n           not shutdownRequested or else probeLate = 0 or else not probeReplayed')
        change('if not probeStarted and then probeSawInput and then backBufferReady and then\n           (for some P of presentations => P.Enabled and then\n             P.Geometry.Scale.Numerator /= P.Geometry.Scale.Denominator) and then', 'if not probeStarted and then backBufferReady and then')
        change('debugPrint ("OUTPUT-DRAIN: starting with a real queued frame" & LF);\n            releaseDisplayBuffer;', 'debugPrint ("OUTPUT-DRAIN: starting with a real queued frame" & LF);\n            running := False;')
        change('   if fbBpp = 32 then\n      declare\n         cleared', '   if not probeFinished or else not shutdownRequested or else outputDrainRequested or else\n     outputReopenPending or else sourceRetirementPending or else backBufferReady or else\n     PS.Charged (pixelStorage) /= 0\n   then debugPrint ("OUTPUT-SHUTDOWN: FAIL exit before retirement" & LF); exitCompositor (1); end if;\n   debugPrint ("OUTPUT-SHUTDOWN: PASS drained loop exited" & LF);\n   if fbBpp = 32 then\n      declare\n         cleared')
    if mode == 'partial':

        def change(a, b):
            nonlocal s
            assert s.count(a) == 1, (a[:90], s.count(a))
            s = s.replace(a, b)
        change('            if probeRendererPolls (Output) <= 3 or else probeInputDuring = 0 or else\n              probeLate = 0\n            then Renderer := Desktop_Compositor.Targets_Busy; end if;', '            if probeRendererPolls (Output) <= 7 then Renderer := Desktop_Compositor.Targets_Busy; end if;')
        change('if probeGrantPolls (Output) (B) <= (if Output = 0 then 3 else 7) then\n                  debugPrint', 'if P.Targets (B).Granted and then probeGrantPolls (Output) (B) <= (if Output = 0 then 3 else 7) then\n                  debugPrint')
        change('PS.Charged (pixelStorage) /= 0 or else not outputReopenPending or else\n           probeInputDuring = 0 or else probeLate = 0 or else not probeReplayed', 'PS.Charged (pixelStorage) /= 0 or else outputReopenPending or else\n           backBufferReady or else probeCharge = 0')
        start = s.index('         if not probeStarted and then probeSawInput and then backBufferReady and then')
        end = s.index('         end if;', start) + len('         end if;')
        s = s[:start] + s[end:]
        change('         P.Targets (B).Address := To_Address (Integer_Address (Raw));\n         MG.Create_Via_Capability', '         P.Targets (B).Address := To_Address (Integer_Address (Raw));\n         if Output = 1 and then B = 2 and then not probeStarted then\n            probeStarted := True;\n            probeCharge := PS.Charged (pixelStorage);\n            for O in Output_Index loop probeTokens (O) := CP.Token (presentations (O).Transfer); end loop;\n            if backBufferReady or else not P.Leased or else not P.Targets (1).Granted or else\n              P.Targets (2).Granted or else P.Targets (2).Allocation = PS.No_Ticket\n            then debugPrint ("OUTPUT-PARTIAL: FAIL invalid injection point" & LF); exitCompositor (1); end if;\n            debugPrint ("OUTPUT-PARTIAL: allocated ungranted target; aborting setup" & LF);\n            closeOutput (Output);\n            return;\n         end if;\n         MG.Create_Via_Capability')
        change('         pumpOutputRetirements;\n         if outputReopenPending', '         pumpOutputRetirements;\n         if probeFinished and then not probeReopened then\n            declare Ready : Boolean; begin\n               activateInternalSession (Ready);\n               if not Ready or else not backBufferReady or else outputDrainRequested then\n                  debugPrint ("OUTPUT-PARTIAL: FAIL recovery" & LF); exitCompositor (1);\n               end if;\n               probeReopened := True;\n               debugPrint ("OUTPUT-PARTIAL: PASS recovered internal session" & LF);\n            end;\n         end if;\n         if outputReopenPending')
    return s

def check(serial, mode):
    if mode not in ('scaled', 'shutdown', 'partial'):
        raise ValueError(mode)
    if re.search('OUTPUT-(?:DRAIN|SHUTDOWN|PARTIAL): FAIL', serial) or 'output retirement uncertain' in serial:
        raise ValueError('retirement failure')
    match = re.search('OUTPUT-DRAIN: PASS drained input=\\s*(\\d+) requests=\\s*(\\d+) late=\\s*(\\d+)', serial)
    if not match or serial.count('OUTPUT-DRAIN: PASS drained') != 1:
        raise ValueError('missing or duplicate cleanup commit')
    required = ['OUTPUT-PARTIAL: allocated ungranted target; aborting setup', match[0], 'OUTPUT-PARTIAL: PASS recovered internal session'] if mode == 'partial' else ['OUTPUT-DRAIN: starting with a real queued frame', 'OUTPUT-DRAIN: holding real completion', 'OUTPUT-DRAIN: delivering held completion', match[0]]
    if mode == 'scaled':
        required += ['OUTPUT-DRAIN: PASS restored scaled layout', 'OUTPUT-DRAIN: PASS reopened']
        if int(match[1]) == 0:
            raise ValueError('no input during drain')
    if mode != 'partial' and int(match[3]) == 0:
        raise ValueError('no delayed presentation')
    if mode == 'shutdown':
        required += ['OUTPUT-SHUTDOWN: PASS drained loop exited']
        pid = re.search('Loaded module desktop\\.svc w/ process ID (\\d+)', serial)
        if not pid or 'OUTPUT-DRAIN: PASS reopened' in serial:
            raise ValueError('missing process identity or shutdown reopened')
        required += ['Process.reclaimProcess: stopped PID ' + pid[1] + '\n']
    offset = 0
    for marker in required:
        found = serial.find(marker, offset)
        if found < 0:
            raise ValueError('missing or reordered marker: ' + marker)
        offset = found + len(marker)

def self_test():
    prefix = 'Loaded module desktop.svc w/ process ID 32\nOUTPUT-DRAIN: starting with a real queued frame\nOUTPUT-DRAIN: holding real completion\nOUTPUT-DRAIN: delivering held completion\n'
    commit = 'OUTPUT-DRAIN: PASS drained input= 2 requests= 0 late= 2\n'
    traces = {'scaled': prefix + commit + 'OUTPUT-DRAIN: PASS restored scaled layout\nOUTPUT-DRAIN: PASS reopened\n', 'shutdown': prefix + commit + 'OUTPUT-SHUTDOWN: PASS drained loop exited\nProcess.reclaimProcess: stopped PID 32\n', 'partial': 'OUTPUT-PARTIAL: allocated ungranted target; aborting setup\n' + commit + 'OUTPUT-PARTIAL: PASS recovered internal session\n'}
    rejected = 0
    for mode, good in traces.items():
        check(good, mode)
        bad = [good.replace(line, '', 1) for line in good.splitlines(True) if mode == 'shutdown' or not line.startswith('Loaded module')]
        bad += [good + commit, good + 'OUTPUT-DRAIN: FAIL early storage release\n', good + 'desktop: output retirement uncertain\n']
        if mode == 'scaled':
            bad += [good.replace('input= 2', 'input= 0')]
        if mode != 'partial':
            bad += [good.replace('late= 2', 'late= 0')]
        if mode == 'shutdown':
            bad += [good + 'OUTPUT-DRAIN: PASS reopened\n']
        for text in bad:
            try:
                check(text, mode)
            except ValueError:
                rejected += 1
                continue
            raise AssertionError((mode, 'bad trace accepted'))
    print('PASS output-retirement oracle: three modes and', rejected, 'negative controls')
