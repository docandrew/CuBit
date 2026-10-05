"""Pinned Servo mouse cancellation and input coalescing integration.

Cancellation follows the regular input route so it cannot overtake a press.
The owning view resets document gesture state without a synthetic mouse-up.
"""


def apply(edit):
    edit(
        'components/shared/embedder/input_events.rs',
        '    MouseButton(MouseButtonEvent),',
        """    MouseButton(MouseButtonEvent),
    /// Cancel the mouse gesture without synthesizing a release or activation.
    MouseCancel,""",
        1
    )

    edit(
        'components/shared/embedder/input_events.rs',
        '            InputEvent::MouseLeftViewport(_) => None,',
        '            InputEvent::MouseLeftViewport(_) | InputEvent::MouseCancel => None,',
        1
    )

    edit(
        'components/constellation/constellation.rs',
        """        if let InputEvent::MouseButton(event) = &event.event {
            self.update_pressed_mouse_buttons(event);
        }""",
        """        if let InputEvent::MouseButton(event) = &event.event {
            self.update_pressed_mouse_buttons(event);
        }
        if matches!(event.event, InputEvent::MouseCancel) {
            self.pressed_mouse_buttons = MouseButtons::empty();
        }""",
        1
    )

    edit(
        'components/constellation/constellation_webview.rs',
        """        let Some(pipeline_id) = self.target_pipeline_id_for_input_event(&event, browsing_contexts)
""",
        """        // Cancellation has no hit test: a resize or frame transition may have
        // removed the original target. Reset every document owned by this view,
        // including an old frame which still retains capture or a pressed state.
        // It uses the same paint/constellation input path as presses, preserving
        // order. Give each additional delivery its own acknowledgment identity.
        if matches!(event.event.event, InputEvent::MouseCancel) {
            let mut sent = false;
            for pipeline in pipelines.values().filter(|p| p.webview_id == self.webview_id) {
                let mut reset = event.clone();
                if sent { reset.event = InputEvent::MouseCancel.into(); }
                if pipeline.event_loop.send(ScriptThreadMessage::SendInputEvent(
                    self.webview_id, pipeline.id, reset,
                )).is_ok() { sent = true; }
            }
            return sent;
        }
        let Some(pipeline_id) = self.target_pipeline_id_for_input_event(&event, browsing_contexts)
""",
        1
    )

    edit(
        'components/script/dom/document/document_event_handler.rs',
        '    mouse_button_state: Cell<MouseButtons>,',
        """    mouse_button_state: Cell<MouseButtons>,
    // Retained only during a mouse gesture, for cancellation without a hit test.
    last_pressed_mouse_event: MutNullableDom<MouseEvent>,
    last_pressed_mouse_target: MutNullableDom<EventTarget>,""",
        1
    )

    edit(
        'components/script/dom/document/document_event_handler.rs',
        '            mouse_button_state: Cell::new(MouseButtons::empty()),',
        """            mouse_button_state: Cell::new(MouseButtons::empty()),
            last_pressed_mouse_event: Default::default(),
            last_pressed_mouse_target: Default::default(),""",
        1
    )

    edit(
        'components/script/dom/document/document_event_handler.rs',
        """                InputEvent::MouseMove(_) => {
                    self.handle_native_mouse_move_event(cx, &event);""",
        """                InputEvent::MouseCancel => {
                    self.cancel_mouse_gesture(cx);
                    InputEventResult::default()
                },
                InputEvent::MouseMove(_) => {
                    self.handle_native_mouse_move_event(cx, &event);""",
        1
    )

    edit(
        'components/script/dom/document/document_event_handler.rs',
        '    fn handle_mouse_left_viewport_event(',
        """    fn cancel_mouse_gesture(&self, cx: &mut JSContext) {
        let mouse_event = self.last_pressed_mouse_event.get();
        let target = self.get_pointer_capture_target(PointerId::Mouse as i32)
            .map(DomRoot::upcast::<EventTarget>)
            .or_else(|| self.last_pressed_mouse_target.get());
        // Clear before invoking script: cancellation handlers cannot recapture
        // a pointer whose buttons are no longer pressed.
        self.last_pressed_mouse_event.set(None);
        self.last_pressed_mouse_target.set(None);
        self.last_mouse_button_down_point.set(None);
        self.mouse_button_state.set(MouseButtons::empty());
        self.unset_active_element();
        *self.drag_gesture.safe_borrow_mut(cx.no_gc()) = None;
        *self.click_counting_info.safe_borrow_mut(cx.no_gc()) = Default::default();
        if let (Some(mouse_event), Some(target)) = (mouse_event, target) {
            mouse_event.to_pointer_event(cx, Atom::from("pointercancel"))
                .upcast::<Event>().fire(cx, &target);
        }
        self.implicit_release_pointer_capture(cx, PointerId::Mouse as i32, "mouse", true);
    }

    fn handle_mouse_left_viewport_event(""",
        1
    )

    edit(
        'components/script/dom/document/document_event_handler.rs',
        '        let pointer_event = mouse_event.to_pointer_event(cx, Atom::from("pointermove"));',
        """        if !self.mouse_button_state.get().is_empty() {
            self.last_pressed_mouse_event.set(Some(&mouse_event));
            self.last_pressed_mouse_target.set(Some(&pointer_target));
        }
        let pointer_event = mouse_event.to_pointer_event(cx, Atom::from("pointermove"));""",
        1
    )

    edit(
        'components/script/dom/document/document_event_handler.rs',
        '                // Update button state before firing so setPointerCapture works in handler.',
        """                self.last_pressed_mouse_event.set(Some(&mouse_event));
                self.last_pressed_mouse_target.set(Some(&pointer_target));
                // Update button state before firing so setPointerCapture works in handler.""",
        1
    )

    edit(
        'components/script/dom/document/document_event_handler.rs',
        '                // Process pending pointer capture after decrementing button count, but skip',
        """                if input_event.pressed_mouse_buttons.is_empty() {
                    self.last_pressed_mouse_event.set(None);
                    self.last_pressed_mouse_target.set(None);
                } else {
                    self.last_pressed_mouse_event.set(Some(&mouse_event));
                    self.last_pressed_mouse_target.set(Some(&pointer_target));
                }
                // Process pending pointer capture after decrementing button count, but skip""",
        1
    )

    edit(
        'components/script/dom/event/mouseevent.rs',
        """    pub(crate) fn to_pointer_event(
        &self,
        cx: &mut JSContext,
        event_type: Atom,
    ) -> DomRoot<crate::dom::pointerevent::PointerEvent> {
        // TODO: This function should almost certainly accept an enumn for the event type.
        let is_pointer_down = &*event_type == "pointerdown";
        let is_pointer_move = &*event_type == "pointermove";
        let is_pointer_up = &*event_type == "pointerup";

        // Pressure is 0.5 when button is down, 0.0 when up
        let pressure = if is_pointer_down || (is_pointer_move && !self.buttons.get().is_empty()) {
            0.5
        } else {
            0.0
        };

        let button = if is_pointer_down || is_pointer_up {
            self.button.get()
        } else {
            MouseButton::None
        };

        // https://w3c.github.io/pointerevents/#dfn-attributes-and-default-actions
        // For pointerenter and pointerleave events, the composed [DOM] attribute SHOULD be false;
        // for all other pointer events in the table above, the attribute SHOULD be true.
        let composed = !matches!(&*event_type, "pointerenter" | "pointerleave");

        let window = self.global();
        let window = window.as_window();

        let pointer_event = PointerEvent::new(
            cx,
            window,
            event_type,
            EventBubbles::from(self.upcast::<Event>().Bubbles()),
            EventCancelable::from(self.upcast::<Event>().Cancelable()),
            self.uievent.GetView().as_deref(),
            self.uievent.Detail(),
            Point2D::new(self.ScreenX(), self.ScreenY()),
            Point2D::new(self.ClientX(), self.ClientY()),
            Point2D::new(self.PageX(), self.PageY()),
            self.modifiers.get(),
            button,
            self.buttons.get(),
            self.GetRelatedTarget().as_deref(),
            self.point_in_target.get(),
            PointerId::Mouse as i32, // Mouse pointer ID is always -1
            1,                       // width
            1,                       // height
            pressure,
            0.0,      // tangential_pressure
            0,        // tilt_x
            0,        // tilt_y
            0,        // twist
            PI / 2.0, // altitude_angle (perpendicular to surface)
            0.0,      // azimuth_angle
            DOMString::from_static("mouse"),
            true,   // is_primary (mouse is always primary)
            vec![], // coalesced_events
            vec![], // predicted_events
        );

        pointer_event.upcast::<Event>().set_composed(composed);

        pointer_event
    }

""",
        """    pub(crate) fn to_pointer_event(
        &self,
        cx: &mut JSContext,
        event_type: Atom,
    ) -> DomRoot<crate::dom::pointerevent::PointerEvent> {
        // TODO: This function should almost certainly accept an enumn for the event type.
        let is_pointer_down = &*event_type == "pointerdown";
        let is_pointer_move = &*event_type == "pointermove";
        let is_pointer_up = &*event_type == "pointerup";
        let is_pointer_cancel = &*event_type == "pointercancel";

        // Pressure is 0.5 when button is down, 0.0 when up
        let pressure = if is_pointer_down || (is_pointer_move && !self.buttons.get().is_empty()) {
            0.5
        } else {
            0.0
        };

        let button = if is_pointer_down || is_pointer_up {
            self.button.get()
        } else {
            MouseButton::None
        };

        // https://w3c.github.io/pointerevents/#dfn-attributes-and-default-actions
        // For pointerenter and pointerleave events, the composed [DOM] attribute SHOULD be false;
        // for all other pointer events in the table above, the attribute SHOULD be true.
        let composed = !matches!(&*event_type, "pointerenter" | "pointerleave");

        let window = self.global();
        let window = window.as_window();

        let pointer_event = PointerEvent::new(
            cx,
            window,
            event_type,
            EventBubbles::from(self.upcast::<Event>().Bubbles()),
            if is_pointer_cancel { EventCancelable::NotCancelable }
            else { EventCancelable::from(self.upcast::<Event>().Cancelable()) },
            self.uievent.GetView().as_deref(),
            self.uievent.Detail(),
            Point2D::new(self.ScreenX(), self.ScreenY()),
            Point2D::new(self.ClientX(), self.ClientY()),
            Point2D::new(self.PageX(), self.PageY()),
            self.modifiers.get(),
            button,
            if is_pointer_cancel { MouseButtons::empty() } else { self.buttons.get() },
            self.GetRelatedTarget().as_deref(),
            self.point_in_target.get(),
            PointerId::Mouse as i32, // Mouse pointer ID is always -1
            1,                       // width
            1,                       // height
            pressure,
            0.0,      // tangential_pressure
            0,        // tilt_x
            0,        // tilt_y
            0,        // twist
            PI / 2.0, // altitude_angle (perpendicular to surface)
            0.0,      // azimuth_angle
            DOMString::from_static("mouse"),
            true,   // is_primary (mouse is always primary)
            vec![], // coalesced_events
            vec![], // predicted_events
        );

        pointer_event.upcast::<Event>().set_composed(composed);

        pointer_event
    }

""",
        1
    )

    edit(
        'components/constellation/tracing.rs',
        '                InputEvent::MouseMove(..) => target_variant!("MouseMove"),',
        """                InputEvent::MouseMove(..) => target_variant!("MouseMove"),
                InputEvent::MouseCancel => target_variant!("MouseCancel"),""",
        1
    )

    edit(
        'components/script/dom/document/document_event_handler.rs',
        '        let mut pending_input_events = self.pending_input_events.borrow_mut();',
        """        let mut pending_input_events = self.pending_input_events.borrow_mut();
        // Coalescing cannot move motion or scrolling across a button, cancel,
        // focus, or other input boundary.
        if !matches!(event.event.event, InputEvent::MouseMove(..)) {
            *self.mouse_move_event_index.borrow_mut() = None;
        }
        if !matches!(event.event.event, InputEvent::Wheel(..)) {
            *self.wheel_event_index.borrow_mut() = None;
        }""",
        1
    )

