    fn handle_clear_session_history(&mut self, webview_id: WebViewId) {
        let active: HashSet<_> = self.fully_active_browsing_contexts_iter(webview_id)
            .map(|context| context.pipeline_id).collect();
        let Some(webview) = self.webviews.get_mut(&webview_id) else {
            return;
        };
        // Save the documents made unreachable by clearing history before
        // dropping the diffs. Clearing the vectors alone leaves their pipelines
        // (and script runtimes) in the constellation indefinitely.
        let retired: HashSet<_> = webview.session_history.past.iter()
            .filter_map(|diff| diff.alive_old_pipeline())
            .chain(webview.session_history.future.iter()
                .filter_map(|diff| diff.alive_new_pipeline()))
            .collect();
        webview.session_history.future.clear();
        webview.session_history.past.clear();
        for pipeline_id in retired {
            if active.contains(&pipeline_id) { continue; }
            let Some(pipeline) = self.pipelines.get(&pipeline_id) else { continue; };
            // Closing a parent also closes its descendants. Skip a descendant
            // already removed from its context, or a stale/cross-view entry.
            if pipeline.webview_id != webview_id ||
                !self.browsing_contexts.get(&pipeline.browsing_context_id)
                    .is_some_and(|context| context.pipelines.contains(&pipeline_id)) {
                continue;
            }
            self.close_pipeline(pipeline_id, DiscardBrowsingContext::No, ExitPipelineMode::Normal);
        }
        // Same-document pushState entries own serialized state too. Keep the
        // current state of every fully active frame, discard the other states.
        for pipeline_id in active {
            let Some(pipeline) = self.pipelines.get_mut(&pipeline_id) else { continue; };
            let current = pipeline.history_state_id;
            let states: Vec<_> = pipeline.history_states.iter().copied()
                .filter(|state| Some(*state) != current).collect();
            for state in &states { pipeline.history_states.remove(state); }
            if !states.is_empty() {
                self.send_message_to_pipeline(pipeline_id,
                    ScriptThreadMessage::RemoveHistoryStates(pipeline_id, states),
                    "Cleared session history states");
            }
        }
        self.notify_history_changed(webview_id);
        #[cfg(target_os = "cubit")]
        self.cubit_lifetime_trace("history-cleared");
    }
